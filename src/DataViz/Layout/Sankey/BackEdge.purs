-- | **Back-edges, with a place.** A link that closes a cycle (and every link
-- | from a node to itself is one) is set aside by the layout: it gives no
-- | node its column. It is still part of the picture. This module says which
-- | of the caller's links were set aside, by the caller's own names, where
-- | each stands on its two nodes, and gives one way to draw them: out of the
-- | source's right side, down to a channel under the chart, back along it,
-- | and up into the target's left side.
-- |
-- | ```purescript
-- | let laid = computeLayoutWithConfig rows config
-- | let under = routeUnder { top: chartBottom + 10.0, gap: 5.0 } (backEdges laid)
-- | under.routed <#> \r -> generateBackEdgePath 30.0 r   -- stroke r.backEdge.width wide
-- | ```
module DataViz.Layout.Sankey.BackEdge
  ( BackEdge
  , RoutedBackEdge
  , backEdges
  , backEdgesAmong
  , routeUnder
  , generateBackEdgePath
  ) where

import Prelude

import Data.Array (catMaybes, foldl, snoc, sortWith)
import Data.Number.Format (toString)
import DataViz.Layout.Sankey.Path (findNode)
import DataViz.Layout.Sankey.Types (CycleAnalysis, LinkID(..), SankeyLayoutResult, SankeyNode)

-- | A link the layout set aside. `index`: its place in the caller's rows.
-- | `source`, `target`: the names the caller gave (the same name twice for a
-- | link from a node to itself). `width`: `value` at the chart's scale. It
-- | leaves its source at (`x0`, `y0`), on the node's right side, and enters
-- | its target at (`x1`, `y1`), on the node's left side; both are the centre
-- | of the link, which stands `width / 2` either side, inside the node and
-- | below the node's links that go forward.
type BackEdge =
  { index :: Int
  , source :: String
  , target :: String
  , value :: Number
  , width :: Number
  , x0 :: Number
  , y0 :: Number
  , x1 :: Number
  , y1 :: Number
  }

-- | A back-edge with the channel it runs back along: `channel` is the y of
-- | the channel's centre.
type RoutedBackEdge = { backEdge :: BackEdge, channel :: Number }

-- | The back-edges of a layout, in the order of the caller's rows.
backEdges :: SankeyLayoutResult -> Array BackEdge
backEdges result = backEdgesAmong result.nodes result.cycleAnalysis

-- | The same, from the parts: for a layout whose nodes are held apart from
-- | the result (`Lanes`). `index` is the link's own number.
backEdgesAmong :: Array SankeyNode -> CycleAnalysis -> Array BackEdge
backEdgesAmong nodes analysis =
  sortWith _.index $ catMaybes $ (analysis.endCycles <> analysis.interiorCycles) <#> \link -> ado
    source <- findNode nodes link.sourceIndex
    target <- findNode nodes link.targetIndex
    in
      { index: case link.index of LinkID index -> index
      , source: source.name
      , target: target.name
      , value: link.value
      , width: link.width
      , x0: source.x1
      , y0: link.y0
      , x1: target.x0
      , y1: link.y1
      }

-- | A channel for each back-edge, one under another in the order given,
-- | the first with its upper edge at `top` and `gap` between one and the
-- | next. `bottom` is the lower edge of the last channel (`top` where there
-- | are none), so what is drawn under the channels starts there.
routeUnder :: { top :: Number, gap :: Number } -> Array BackEdge -> { routed :: Array RoutedBackEdge, bottom :: Number }
routeUnder { top, gap } edges = { routed: stacked.routed, bottom: max top (stacked.next - gap) }
  where
  stacked = foldl
    (\acc backEdge -> { routed: snoc acc.routed { backEdge, channel: acc.next + backEdge.width / 2.0 }, next: acc.next + backEdge.width + gap })
    { routed: [], next: top }
    edges

-- | The centre line of a routed back-edge, to be stroked `width` wide: out
-- | of the source's right side, round and down to the channel, back along
-- | it, and round and up into the target's left side. `reach`: how far each
-- | turn swings out beyond the node's side.
generateBackEdgePath :: Number -> RoutedBackEdge -> String
generateBackEdgePath reach { backEdge, channel } =
  "M" <> at backEdge.x0 backEdge.y0
    <> " C" <> at (backEdge.x0 + reach) backEdge.y0 <> " " <> at (backEdge.x0 + reach) channel <> " " <> at backEdge.x0 channel
    <> " L" <> at backEdge.x1 channel
    <> " C" <> at (backEdge.x1 - reach) channel <> " " <> at (backEdge.x1 - reach) backEdge.y1 <> " " <> at backEdge.x1 backEdge.y1
  where
  at x y = toString x <> "," <> toString y
