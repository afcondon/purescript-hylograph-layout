-- | **Lanes for long links.** A link that spans more than one column is, in
-- | plain Sankey layout, drawn straight across the columns between, through
-- | whatever nodes stand there. The standard remedy (Sugiyama's dummy nodes)
-- | is to give such a link a waypoint in every column it passes: the layout
-- | then keeps room for it in each, as for any node, and the link is drawn
-- | as one ribbon through its waypoints.
-- |
-- | `computeLayoutWithLanes` does this for the links the caller chooses. It
-- | lays the graph out once to learn each node's column, adds the waypoints,
-- | and lays it out again with every column held, so the chart keeps the
-- | columns it would have had. Back-edges get no lane.
-- |
-- | A waypoint's name is private (`laneOf` reads it). A caller with its own
-- | `nodeSort` sees waypoints in the comparisons and can place a lane, for
-- | example with the node its link comes from: `laneOf` says which link and
-- | which column.
-- |
-- | Built for Triggerfish's signal-flow chart (2026-10-04), where a machine
-- | upstream of another sends notes through that machine's column.
module DataViz.Layout.Sankey.Lanes
  ( Route
  , LaneLayout
  , computeLayoutWithLanes
  , laneOf
  , generateRoutePath
  ) where

import Prelude

import Data.Array (catMaybes, concat, drop, filter, findIndex, foldl, head, index, last, length, mapWithIndex, range, reverse, zipWith)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..))
import Data.String (Pattern(..), split, stripPrefix)
import DataViz.Layout.Sankey.Compute (computeLayoutWithConfig)
import DataViz.Layout.Sankey.Path (findNode)
import DataViz.Layout.Sankey.Types (CycleAnalysis, LinkCSVRow, LinkID(..), SankeyConfig, SankeyLink, SankeyNode)

-- | One input link, as drawn: its segments in order, from its source through
-- | its waypoints (if any) to its target. `index` is its place in the input.
type Route = { index :: Int, segments :: Array SankeyLink }

-- | `nodes`: the graph's own nodes, without waypoints. `waypoints`: the lane
-- | nodes, for drawing routes (pass `nodes <> waypoints` to
-- | `generateRoutePath`). `routes`: one per input link, in input order.
type LaneLayout =
  { nodes :: Array SankeyNode
  , waypoints :: Array SankeyNode
  , routes :: Array Route
  , cycleAnalysis :: CycleAnalysis
  }

prefix :: String
prefix = "\x00lane:"

waypointName :: Int -> Int -> String
waypointName i layer = prefix <> show i <> ":" <> show layer

-- | Whether a node name is a waypoint, and if so of which input link, in
-- | which column.
laneOf :: String -> Maybe { link :: Int, layer :: Int }
laneOf name = do
  rest <- stripPrefix (Pattern prefix) name
  case split (Pattern ":") rest of
    [ i, k ] -> { link: _, layer: _ } <$> Int.fromString i <*> Int.fromString k
    _ -> Nothing

-- | The layout, with a lane for every input link that spans more than one
-- | column and that `wants` (given its input index and row).
computeLayoutWithLanes
  :: (Int -> LinkCSVRow -> Boolean)
  -> Array LinkCSVRow
  -> SankeyConfig
  -> LaneLayout
computeLayoutWithLanes wants rows config =
  { nodes: filter (isNothing <<< laneOf <<< _.name) laid.nodes
  , waypoints: filter (not <<< isNothing <<< laneOf <<< _.name) laid.nodes
  , routes
  , cycleAnalysis: laid.cycleAnalysis
  }
  where
  first = computeLayoutWithConfig rows config
  layers = Map.fromFoldable (map (\n -> Tuple n.name n.layer) first.nodes)

  split' i row = case Map.lookup row.s layers, Map.lookup row.t layers of
    Just a, Just b | b - a > 1 && wants i row ->
      let names = [ row.s ] <> map (waypointName i) (range (a + 1) (b - 1)) <> [ row.t ]
      in zipWith (\s t -> { s, t, v: row.v, origin: i }) names (drop 1 names)
    _, _ -> [ { s: row.s, t: row.t, v: row.v, origin: i } ]
  segs = concat (mapWithIndex split' rows)

  laid = computeLayoutWithConfig (map (\x -> { s: x.s, t: x.t, v: x.v }) segs)
    config
      { nodeLayer = \name -> case laneOf name of
          Just w -> Just w.layer
          Nothing -> Map.lookup name layers
      }

  -- segment k of the input to the second pass is the link with index k
  routes = mapWithIndex route rows
  route i _ =
    { index: i
    , segments: catMaybes (map linkAt (positions i))
    }
  positions i = map _.k (filter (\x -> x.origin == i) (mapWithIndex (\k x -> { k, origin: x.origin }) segs))
  linkAt k = findIndex (\l -> l.index == LinkID k) laid.links >>= index laid.links

-- | A route as one closed ribbon: the top edge forward through each segment
-- | and across each waypoint, the bottom edge back. `nodes` must include the
-- | waypoints.
generateRoutePath :: Array SankeyNode -> Route -> String
generateRoutePath nodes r = case head r.segments, last r.segments of
  Just _, Just _ ->
    let
      spans = catMaybes (map span r.segments)
      top = foldl (\acc s -> acc <> (if acc == "" then "M" <> pt s.x0 (s.y0 - s.w) else " L" <> pt s.x0 (s.y0 - s.w)) <> curve s.x0 s.x1 (s.y0 - s.w) (s.y1 - s.w)) "" spans
      bottom = foldl (\acc s -> acc <> " L" <> pt s.x1 (s.y1 + s.w) <> curve s.x1 s.x0 (s.y1 + s.w) (s.y0 + s.w)) "" (reverse spans)
    in
      if length spans == 0 then "" else top <> bottom <> " Z"
  _, _ -> ""
  where
  span l = do
    src <- findNode nodes l.sourceIndex
    tgt <- findNode nodes l.targetIndex
    pure { x0: src.x1, x1: tgt.x0, y0: l.y0, y1: l.y1, w: l.width / 2.0 }
  curve xa xb ya yb =
    let xi = (xa + xb) / 2.0
    in " C" <> pt xi ya <> " " <> pt xi yb <> " " <> pt xb yb
  pt x y = toString x <> "," <> toString y
