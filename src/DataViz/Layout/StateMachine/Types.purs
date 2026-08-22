-- | DataViz.Layout.StateMachine.Types
-- |
-- | Types for state machine visualization.
-- | A state machine diagram shows states as circles/ovals
-- | and transitions as labeled arrows between them.
-- |
-- | Both states and transitions carry an `extra` payload, and the two are
-- | independently parameterised: a state's payload describes a state (a phase,
-- | a liveness flag), a transition's payload describes an edge (the event that
-- | fires it, a guard, a refusal reason). A caller that wants neither uses
-- | `Unit` for both.
module DataViz.Layout.StateMachine.Types
  ( State
  , Transition
  , StateMachine
  , StatePosition
  , TransitionPath
  , LayoutState
  , LayoutTransition
  , StateMachineLayout
  ) where

-- | A state in the state machine
-- | The `extra` field allows attaching arbitrary data (e.g., phantom type info)
type State extra =
  { id :: String           -- Unique identifier
  , label :: String        -- Display label
  , isInitial :: Boolean   -- Has incoming arrow from nowhere
  , isFinal :: Boolean     -- Double circle
  , extra :: extra         -- User-defined extra data
  }

-- | A transition between states
-- | The `extra` field carries whatever the caller needs to keep attached to the
-- | edge itself — the triggering event, a guard, a refusal reason — so that a
-- | renderer can style or annotate an arrow without a side table keyed by
-- | `from`/`to`/`label`.
type Transition extra =
  { from :: String         -- Source state id
  , to :: String           -- Target state id
  , label :: String        -- Transition label (e.g., operation name)
  , extra :: extra         -- User-defined extra data
  }

-- | Complete state machine definition
type StateMachine stateExtra transitionExtra =
  { states :: Array (State stateExtra)
  , transitions :: Array (Transition transitionExtra)
  }

-- | Computed position for a state
type StatePosition =
  { cx :: Number           -- Center x
  , cy :: Number           -- Center y
  , rx :: Number           -- Horizontal radius (for ellipse)
  , ry :: Number           -- Vertical radius (for ellipse)
  }

-- | Computed path for a transition arrow
-- | Uses quadratic bezier for curved arrows
type TransitionPath =
  { startX :: Number       -- Arrow start point
  , startY :: Number
  , controlX :: Number     -- Bezier control point
  , controlY :: Number
  , endX :: Number         -- Arrow end point (at state edge)
  , endY :: Number
  , labelX :: Number       -- Position for the label
  , labelY :: Number
  , angle :: Number        -- Angle at endpoint for arrowhead
  , isSelfLoop :: Boolean  -- Special rendering for self-transitions
  }

-- | A state with computed layout
type LayoutState extra =
  { state :: State extra
  , position :: StatePosition
  }

-- | A transition with computed layout
type LayoutTransition extra =
  { transition :: Transition extra
  , path :: TransitionPath
  }

-- | Complete layout output ready for rendering
-- | `originX`/`originY` are the top-left of the drawing, which is NOT always
-- | (0, 0): a self-loop or a transition label can sit left of or above the
-- | leftmost state, and a viewBox pinned to the origin clips them. Renderers
-- | should use all four as the viewBox.
type StateMachineLayout stateExtra transitionExtra =
  { states :: Array (LayoutState stateExtra)
  , transitions :: Array (LayoutTransition transitionExtra)
  , originX :: Number
  , originY :: Number
  , width :: Number
  , height :: Number
  , initialArrow :: { x :: Number, y :: Number, angle :: Number }  -- Arrow pointing to initial state
  }
