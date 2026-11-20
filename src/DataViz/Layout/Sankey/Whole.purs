-- | **The whole picture.** The Sankey layout places the nodes that links
-- | mention and routes the links that go forward. Two things are left over:
-- | the links that close a cycle, and the nodes that no link mentions at
-- | all. `computeWholeLayout` places those too, under the chart, and leaves
-- | the chart itself exactly as `computeLayoutWithLanes` (or
-- | `computeLayoutWithLanesAtScale`) gives it:
-- |
-- | ```
-- |   extent.y0  ┌──────────── the chart: columns, lanes ───────────┐
-- |              └──────────────────────────────────────────────────┘
-- |              backEdges.margin
-- |              ══ a channel for each back-edge, in input order ════
-- |              shelf.margin
-- |              ▭ ▭ ▭ ▭   the shelf: the isolated nodes, in the order
-- |              ▭ ▭       given, in rows, left to right from extent.x0
-- | ```
-- |
-- | A node that only a link to itself mentions is not isolated: it stands in
-- | the chart, in the first column, as tall as that link is wide.
module DataViz.Layout.Sankey.Whole
  ( Sizing(..)
  , BackEdgeRouting
  , ShelfOptions
  , WholeOptions
  , IsolatedNode
  , Extent
  , WholeLayout
  , computeWholeLayout
  , isolatedAmong
  ) where

import Prelude

import Data.Array (filter, foldl, mapWithIndex, nub, null)
import Data.Int (toNumber)
import Data.Set as Set
import DataViz.Layout.Sankey.BackEdge (RoutedBackEdge, routeUnder)
import DataViz.Layout.Sankey.Compute (Scale)
import DataViz.Layout.Sankey.Lanes (Route, computeLayoutWithLanes, computeLayoutWithLanesAtScale)
import DataViz.Layout.Sankey.Types (CycleAnalysis, LinkCSVRow, SankeyConfig, SankeyNode)

-- | How the chart gets its size. `FitExtent`: it fills `config.extent`, as
-- | the Sankey layout always has. `AtScale`: `config.extent` gives only its
-- | top left corner, and it is as large as the scale makes it.
data Sizing
  = FitExtent
  | AtScale Scale

-- | `margin`: from the bottom of the chart to the first channel. `gap`:
-- | between one channel and the next. `reach`: how far a back-edge swings
-- | out beyond a node's side as it turns (`generateBackEdgePath`).
type BackEdgeRouting = { margin :: Number, gap :: Number, reach :: Number }

-- | `margin`: from what is above the shelf (the channels, or the chart) to
-- | its first row. A row holds as many nodes as the chart has columns, and
-- | at least `minimumColumns`. `columnPitch`: from the left of one node to
-- | the left of the next. Each node is `config.nodeWidth` wide and
-- | `nodeHeight` tall, with `rowGap` between rows.
type ShelfOptions =
  { margin :: Number
  , minimumColumns :: Int
  , columnPitch :: Number
  , nodeHeight :: Number
  , rowGap :: Number
  }

type WholeOptions =
  { sizing :: Sizing
  , backEdges :: BackEdgeRouting
  , shelf :: ShelfOptions
  }

-- | A node that no link mentions, on the shelf.
type IsolatedNode =
  { name :: String
  , x0 :: Number
  , y0 :: Number
  , x1 :: Number
  , y1 :: Number
  , row :: Int
  , column :: Int
  }

type Extent = { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }

-- | `nodes`, `waypoints`, `routes`, `cycleAnalysis`: as in `LaneLayout`.
-- | `backEdges`: the links set aside, each with its channel, in input order.
-- | `isolated`: the nodes no link mentions, in the order given. `chart`: the
-- | box around the nodes and waypoints. `extent`: the box around everything,
-- | the turns of the back-edges included.
type WholeLayout =
  { nodes :: Array SankeyNode
  , waypoints :: Array SankeyNode
  , routes :: Array Route
  , cycleAnalysis :: CycleAnalysis
  , backEdges :: Array RoutedBackEdge
  , isolated :: Array IsolatedNode
  , chart :: Extent
  , extent :: Extent
  }

-- | The names among `names` that no row mentions, each once, in order.
isolatedAmong :: Array String -> Array LinkCSVRow -> Array String
isolatedAmong names rows = filter (\name -> not (Set.member name mentioned)) (nub names)
  where
  mentioned = Set.fromFoldable (rows >>= \row -> [ row.s, row.t ])

-- | The layout of `rows`, with lanes for the long links that `wants`
-- | chooses, and with a place for every back-edge and for every name in
-- | `names` that no row mentions. `names` may be all of the caller's nodes
-- | or only the ones that may be isolated; with none, and with `FitExtent`,
-- | the nodes, waypoints and routes are `computeLayoutWithLanes`'s.
computeWholeLayout
  :: WholeOptions
  -> (Int -> LinkCSVRow -> Boolean)
  -> Array String
  -> Array LinkCSVRow
  -> SankeyConfig
  -> WholeLayout
computeWholeLayout options wants names rows config =
  { nodes: laid.nodes
  , waypoints: laid.waypoints
  , routes: laid.routes
  , cycleAnalysis: laid.cycleAnalysis
  , backEdges: under.routed
  , isolated
  , chart
  , extent:
      { x0: foldl (\acc r -> min acc (r.backEdge.x1 - options.backEdges.reach)) chart.x0 under.routed
      , y0: chart.y0
      , x1: foldl (\acc r -> max acc (r.backEdge.x0 + options.backEdges.reach)) (foldl (\acc node -> max acc node.x1) chart.x1 isolated) under.routed
      , y1: foldl (\acc node -> max acc node.y1) under.bottom isolated
      }
  }
  where
  laid = case options.sizing of
    FitExtent -> computeLayoutWithLanes wants rows config
    AtScale scale -> computeLayoutWithLanesAtScale scale wants rows config

  boxes = laid.nodes <> laid.waypoints
  chart =
    { x0: config.extent.x0
    , y0: config.extent.y0
    , x1: foldl (\acc node -> max acc node.x1) config.extent.x0 boxes
    , y1: foldl (\acc node -> max acc node.y1) config.extent.y0 boxes
    }

  under =
    if null laid.backEdges then { routed: [], bottom: chart.y1 }
    else routeUnder { top: chart.y1 + options.backEdges.margin, gap: options.backEdges.gap } laid.backEdges

  chartColumns = if null boxes then 0 else foldl (\acc node -> max acc node.layer) 0 boxes + 1
  perRow = max 1 (max options.shelf.minimumColumns chartColumns)
  shelfTop = if null boxes then config.extent.y0 else under.bottom + options.shelf.margin
  isolated = isolatedAmong names rows # mapWithIndex \place name ->
    let
      column = place `mod` perRow
      row = place / perRow
      x0 = config.extent.x0 + toNumber column * options.shelf.columnPitch
      y0 = shelfTop + toNumber row * (options.shelf.nodeHeight + options.shelf.rowGap)
    in
      { name, x0, y0, x1: x0 + config.nodeWidth, y1: y0 + options.shelf.nodeHeight, row, column }
