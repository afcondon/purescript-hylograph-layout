-- | DataViz.Layout.StateMachine.Layout
-- |
-- | Pure layout algorithms for state machine visualization.
-- | Arranges states in a circular layout and computes curved
-- | arrow paths for transitions.
module DataViz.Layout.StateMachine.Layout
  ( layout
  , layoutWithConfig
  , LayoutConfig
  , defaultConfig
  , circularLayout
  , gridLayout
  , ParallelInfo
  ) where

import Prelude

import Data.Array (catMaybes, elemIndex, filter, length, mapWithIndex, zipWith)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin, sqrt, atan2)
import DataViz.Layout.StateMachine.Types (State, Transition, StateMachine, StatePosition, TransitionPath, LayoutState, LayoutTransition, StateMachineLayout)

-- | Configuration for state machine layout
type LayoutConfig =
  { stateRadiusX :: Number      -- Horizontal radius for state ellipse
  , stateRadiusY :: Number      -- Vertical radius for state ellipse
  , layoutRadius :: Number      -- Radius of circular arrangement
  , margin :: Number            -- Margin around the diagram
  , selfLoopRadius :: Number    -- Radius for self-loop curves
  , arrowOffset :: Number       -- Gap between arrow and state edge
  , initialArrowLength :: Number -- Length of arrow pointing to initial state
  , parallelSeparation :: Number -- Gap between arcs that share a pair of states
  , minPairedCurvature :: Number -- Floor on the bow when the reverse edge also
                                 -- exists, so A->B and B->A cannot collapse onto
                                 -- one line however flat the base curve is
  , labelOffset :: Number      -- How far a label sits off its arc, perpendicular
  , edgeCurvature :: Number    -- How far an arc bows off the straight line,
                               -- as a fraction of its length. A ring needs a
                               -- generous bow to keep parallel chords apart;
                               -- a tidy tree wants its parent-child links
                               -- nearly straight or the labels collide.
  }

-- | Default layout configuration
defaultConfig :: LayoutConfig
defaultConfig =
  { stateRadiusX: 40.0
  , stateRadiusY: 25.0
  , layoutRadius: 150.0
  , margin: 80.0
  , selfLoopRadius: 50.0
  , arrowOffset: 3.0
  , initialArrowLength: 40.0
  , parallelSeparation: 24.0
  , minPairedCurvature: 20.0
  , labelOffset: 12.0
  , edgeCurvature: 0.15
  }

-- | Where a transition sits among those sharing its endpoints.
-- |
-- | `index`/`count` place same-direction siblings side by side. `hasOpposite`
-- | is the antiparallel case and needs different treatment: A->B and B->A
-- | already bow to opposite sides of the line (the perpendicular is computed
-- | from the travel direction, which is reversed), so they want the *same*
-- | positive magnitude rather than opposite offsets — they just need that
-- | magnitude floored so the gap survives a flat base curve.
type ParallelInfo =
  { index :: Int
  , count :: Int
  , hasOpposite :: Boolean
  }

parallelInfo :: forall te. Array (Transition te) -> Array ParallelInfo
parallelInfo transitions = mapWithIndex info transitions
  where
  info i t =
    let
      mates = catMaybes $
        mapWithIndex (\j s -> if s.from == t.from && s.to == t.to then Just j else Nothing)
          transitions
      opposite = Array.any (\s -> s.from == t.to && s.to == t.from) transitions
    in
      { index: fromMaybe 0 (elemIndex i mates)
      , count: length mates
      , hasOpposite: opposite && t.from /= t.to
      }

-- | Layout a state machine with default configuration using circular layout
layout :: forall se te. StateMachine se te -> StateMachineLayout se te
layout = layoutWithConfig defaultConfig circularLayout

-- | Layout with custom configuration and layout strategy
layoutWithConfig :: forall se te.
  LayoutConfig ->
  (LayoutConfig -> Array (State se) -> Array (LayoutState se)) ->
  StateMachine se te ->
  StateMachineLayout se te
layoutWithConfig config layoutFn machine =
  let
    -- Position states
    layoutStates = layoutFn config machine.states

    -- Compute transitions, each knowing who shares its endpoints
    layoutTransitions =
      zipWith (layoutTransition config layoutStates)
        (parallelInfo machine.transitions)
        machine.transitions

    -- Find initial state for the entry arrow
    initialArrow = computeInitialArrow config layoutStates

    -- Compute bounding box over everything that will actually be drawn
    { originX, originY, width, height } =
      computeBounds config layoutStates layoutTransitions initialArrow
  in
    { states: layoutStates
    , transitions: layoutTransitions
    , originX
    , originY
    , width
    , height
    , initialArrow
    }

-- | Circular layout: arrange states in a circle
circularLayout :: forall extra. LayoutConfig -> Array (State extra) -> Array (LayoutState extra)
circularLayout config states =
  let
    n = length states
    angleStep = if n > 0 then 2.0 * pi / toNumber n else 0.0
    centerX = config.layoutRadius + config.margin
    centerY = config.layoutRadius + config.margin
  in
    mapWithIndex (positionState centerX centerY angleStep) states
  where
  positionState :: Number -> Number -> Number -> Int -> State extra -> LayoutState extra
  positionState cx cy step idx state =
    let
      -- Start from top (-pi/2) and go clockwise
      angle = -pi / 2.0 + toNumber idx * step
      position =
        { cx: cx + config.layoutRadius * cos angle
        , cy: cy + config.layoutRadius * sin angle
        , rx: config.stateRadiusX
        , ry: config.stateRadiusY
        }
    in
      { state, position }

-- | Grid layout: arrange states in rows
gridLayout :: forall extra. LayoutConfig -> Array (State extra) -> Array (LayoutState extra)
gridLayout config states =
  let
    n = length states
    cols = max 1 (ceil (sqrt (toNumber n)))
    cellWidth = 2.0 * config.stateRadiusX + 40.0
    cellHeight = 2.0 * config.stateRadiusY + 60.0
  in
    mapWithIndex (positionState cols cellWidth cellHeight) states
  where
  positionState :: Int -> Number -> Number -> Int -> State extra -> LayoutState extra
  positionState cols cellW cellH idx state =
    let
      col = idx `mod` cols
      row = idx / cols
      position =
        { cx: config.margin + toNumber col * cellW + cellW / 2.0
        , cy: config.margin + toNumber row * cellH + cellH / 2.0
        , rx: config.stateRadiusX
        , ry: config.stateRadiusY
        }
    in
      { state, position }

-- | Compute a transition path between two states
layoutTransition :: forall se te.
  LayoutConfig ->
  Array (LayoutState se) ->
  ParallelInfo ->
  Transition te ->
  LayoutTransition te
layoutTransition config states parallel transition =
  let
    fromPos = findStatePosition states transition.from
    toPos = findStatePosition states transition.to
    -- Compute layout center as centroid of all states
    layoutCenter = computeLayoutCenter states
    path = case fromPos, toPos of
      Just from, Just to ->
        if transition.from == transition.to
          then selfLoopPath config layoutCenter from
          else arcPath config from to parallel
      _, _ -> defaultPath
  in
    { transition, path }

-- | Compute the centroid of all states (layout center)
computeLayoutCenter :: forall extra. Array (LayoutState extra) -> { cx :: Number, cy :: Number }
computeLayoutCenter states =
  let
    n = toNumber (length states)
    sumX = foldl (\acc s -> acc + s.position.cx) 0.0 states
    sumY = foldl (\acc s -> acc + s.position.cy) 0.0 states
  in
    if n > 0.0
      then { cx: sumX / n, cy: sumY / n }
      else { cx: 0.0, cy: 0.0 }

-- | Find the position of a state by id
findStatePosition :: forall extra. Array (LayoutState extra) -> String -> Maybe StatePosition
findStatePosition states id =
  case filter (\s -> s.state.id == id) states of
    [s] -> Just s.position
    _ -> Nothing

-- | Compute arc path between two different states
arcPath :: LayoutConfig -> StatePosition -> StatePosition -> ParallelInfo -> TransitionPath
arcPath config from to parallel =
  let
    -- Vector from source to target
    dx = to.cx - from.cx
    dy = to.cy - from.cy
    dist = sqrt (dx * dx + dy * dy)

    -- Normalized direction
    nx = if dist > 0.0 then dx / dist else 1.0
    ny = if dist > 0.0 then dy / dist else 0.0

    -- Start point: edge of source ellipse
    startX = from.cx + nx * (from.rx + config.arrowOffset)
    startY = from.cy + ny * (from.ry + config.arrowOffset)

    -- End point: edge of target ellipse
    endX = to.cx - nx * (to.rx + config.arrowOffset)
    endY = to.cy - ny * (to.ry + config.arrowOffset)

    -- Control point: perpendicular offset for curve
    -- More curvature for longer distances
    -- Positive perpendicular = curve bows to the right (clockwise)
    base = min 30.0 (dist * config.edgeCurvature)
    -- Floor the bow when the reverse edge exists, or the two arcs and their two
    -- labels land on top of each other.
    floored = if parallel.hasOpposite then max base config.minPairedCurvature else base
    -- Same-direction siblings fan out either side of that.
    slot = toNumber parallel.index - toNumber (parallel.count - 1) / 2.0
    curveAmount = floored + slot * config.parallelSeparation

    perpX = ny * curveAmount   -- Flipped sign for clockwise curve
    perpY = -nx * curveAmount  -- Flipped sign for clockwise curve
    midX = (startX + endX) / 2.0
    midY = (startY + endY) / 2.0
    controlX = midX + perpX
    controlY = midY + perpY

    -- Label sits off the arc along the same perpendicular, so it never lies on
    -- the line it is naming.
    labelSign = if curveAmount < 0.0 then -1.0 else 1.0
    labelX = controlX + ny * config.labelOffset * labelSign
    labelY = controlY - nx * config.labelOffset * labelSign

    -- Angle at endpoint for arrowhead
    -- Tangent of quadratic bezier at t=1 is (end - control)
    tangentX = endX - controlX
    tangentY = endY - controlY
    angle = atan2 tangentY tangentX
  in
    { startX, startY, controlX, controlY, endX, endY, labelX, labelY, angle, isSelfLoop: false }

-- | Compute self-loop path (arrow from state back to itself)
-- | Start and end points are on the state's edge, arc bulges outward
selfLoopPath :: LayoutConfig -> { cx :: Number, cy :: Number } -> StatePosition -> TransitionPath
selfLoopPath config layoutCenter pos =
  let
    loopRadius = config.selfLoopRadius * 0.5  -- Radius of the arc

    -- Direction from layout center to this state (outward direction)
    dx = pos.cx - layoutCenter.cx
    dy = pos.cy - layoutCenter.cy
    dist = sqrt (dx * dx + dy * dy)

    -- Outward angle (direction away from center)
    outwardAngle = if dist > 0.0 then atan2 dy dx else -pi / 2.0

    -- Start and end points on the STATE's edge, spread around the outward direction
    spread = 0.6  -- radians of spread on the state's edge
    startAngle = outwardAngle - spread
    endAngle = outwardAngle + spread

    -- Points where the loop meets the state's edge
    startX = pos.cx + pos.rx * cos startAngle
    startY = pos.cy + pos.ry * sin startAngle
    endX = pos.cx + pos.rx * cos endAngle
    endY = pos.cy + pos.ry * sin endAngle

    -- Control point: used for label positioning (apex of the arc)
    -- The arc bulges outward from the midpoint between start and end
    controlX = pos.cx + (pos.rx + loopRadius * 1.5) * cos outwardAngle
    controlY = pos.cy + (pos.ry + loopRadius * 1.5) * sin outwardAngle

    -- Label at the apex of the loop
    labelX = pos.cx + (pos.rx + loopRadius + 14.0) * cos outwardAngle
    labelY = pos.cy + (pos.ry + loopRadius + 14.0) * sin outwardAngle

    -- Arrowhead angle: tangent to arc at end point, pointing into the state
    -- For clockwise arc, tangent points perpendicular to the radius
    angle = endAngle - pi / 2.0
  in
    { startX, startY, controlX, controlY, endX, endY, labelX, labelY, angle, isSelfLoop: true }

-- | Default path for missing states
defaultPath :: TransitionPath
defaultPath =
  { startX: 0.0, startY: 0.0
  , controlX: 0.0, controlY: 0.0
  , endX: 0.0, endY: 0.0
  , labelX: 0.0, labelY: 0.0
  , angle: 0.0
  , isSelfLoop: false
  }

-- | Compute the initial state entry arrow
computeInitialArrow :: forall extra. LayoutConfig -> Array (LayoutState extra) -> { x :: Number, y :: Number, angle :: Number }
computeInitialArrow config states =
  case filter (\s -> s.state.isInitial) states of
    [initial] ->
      let
        -- Arrow comes from the left
        x = initial.position.cx - initial.position.rx - config.initialArrowLength
        y = initial.position.cy
        angle = 0.0  -- Points right
      in
        { x, y, angle }
    _ -> { x: 0.0, y: 0.0, angle: 0.0 }

-- | Compute the bounding box for the diagram.
-- |
-- | Measures the states, but also every arc's control point, endpoints and
-- | label anchor, and the initial arrow. A self-loop bulges outward and its
-- | label sits further out still, so a box drawn from the state ellipses alone
-- | clips exactly the annotations a reader most needs.
computeBounds
  :: forall se te
   . LayoutConfig
  -> Array (LayoutState se)
  -> Array (LayoutTransition te)
  -> { x :: Number, y :: Number, angle :: Number }
  -> { originX :: Number, originY :: Number, width :: Number, height :: Number }
computeBounds config states transitions initialArrow =
  let
    seed = { minX: initialArrow.x, minY: initialArrow.y, maxX: initialArrow.x, maxY: initialArrow.y }

    widen acc p =
      { minX: min acc.minX p.x
      , minY: min acc.minY p.y
      , maxX: max acc.maxX p.x
      , maxY: max acc.maxY p.y
      }

    statePoints = Array.concatMap
      (\s ->
        [ { x: s.position.cx - s.position.rx, y: s.position.cy - s.position.ry }
        , { x: s.position.cx + s.position.rx, y: s.position.cy + s.position.ry }
        ]
      )
      states

    -- A label is text, so allow for it spreading either side of its anchor.
    labelPad = 34.0
    transitionPoints = Array.concatMap
      (\t ->
        [ { x: t.path.startX, y: t.path.startY }
        , { x: t.path.endX, y: t.path.endY }
        , { x: t.path.controlX, y: t.path.controlY }
        , { x: t.path.labelX - labelPad, y: t.path.labelY - 10.0 }
        , { x: t.path.labelX + labelPad, y: t.path.labelY + 10.0 }
        ]
      )
      transitions

    bounds = foldl widen seed (statePoints <> transitionPoints)
  in
    { originX: bounds.minX - config.margin
    , originY: bounds.minY - config.margin
    , width: bounds.maxX - bounds.minX + 2.0 * config.margin
    , height: bounds.maxY - bounds.minY + 2.0 * config.margin
    }

-- | Ceiling function for integers
ceil :: Number -> Int
ceil n =
  let i = floor n
  in if toNumber i < n then i + 1 else i

-- | Floor function
floor :: Number -> Int
floor n =
  let i = truncate n
  in if toNumber i > n then i - 1 else i

-- | Truncate to integer
truncate :: Number -> Int
truncate n = if n >= 0.0 then truncatePos n else -(truncatePos (-n))
  where
  truncatePos :: Number -> Int
  truncatePos x = go 0 x
    where
    go acc remaining
      | remaining < 1.0 = acc
      | otherwise = go (acc + 1) (remaining - 1.0)
