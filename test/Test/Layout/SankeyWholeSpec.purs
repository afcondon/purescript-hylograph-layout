-- | Test.Layout.SankeyWholeSpec
-- |
-- | The Sankey layout on the inputs it used to mishandle: a link from a node
-- | to itself, nodes that no link mentions, and links that close a cycle.
-- | With them, a golden file at full precision (no rounding) of what the
-- | layout gave for inputs with none of those, written before any of that
-- | was changed, so that the change is shown to leave them alone.
module Test.Layout.SankeyWholeSpec
  ( runSankeyWholeTests
  , sankeyWholeTestCount
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, stringify)
import Data.Array as Array
import Data.Foldable (all, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (abs, isFinite)
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.Tuple.Nested ((/\))
import DataViz.Layout.Sankey.BackEdge (BackEdge, backEdges, generateBackEdgePath, routeUnder)
import DataViz.Layout.Sankey.Compute (computeLayoutWithConfig, configAtScale)
import DataViz.Layout.Sankey.Lanes (LaneLayout, computeLayoutWithLanes, computeLayoutWithLanesAtScale)
import DataViz.Layout.Sankey.Whole (Sizing(..), WholeOptions, computeWholeLayout, isolatedAmong)
import DataViz.Layout.Sankey.Types (Alignment(..), LinkCSVRow, LinkID(..), NodeID(..), SankeyConfig, SankeyLayoutResult, SankeyLink, SankeyNode, defaultSankeyConfig)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Nine nodes in four columns, with three links that span more than one
-- | column, two sources and two sinks. No cycle, no link from a node to
-- | itself.
acyclicRows :: Array LinkCSVRow
acyclicRows =
  [ { s: "wheat", t: "mill", v: 12.0 }
  , { s: "wheat", t: "feed", v: 3.5 }
  , { s: "rye", t: "mill", v: 4.0 }
  , { s: "mill", t: "flour", v: 11.0 }
  , { s: "mill", t: "bran", v: 5.0 }
  , { s: "flour", t: "bread", v: 9.0 }
  , { s: "flour", t: "pasta", v: 2.0 }
  , { s: "bran", t: "feed", v: 5.0 }
  , { s: "wheat", t: "bread", v: 1.25 }
  , { s: "rye", t: "bread", v: 2.75 }
  , { s: "mill", t: "bread", v: 0.5 }
  ]

-- | A graph with two links that close cycles. Only its nodes and forward
-- | links are in the golden file: what its closing links are given is what
-- | changed.
cyclicRows :: Array LinkCSVRow
cyclicRows =
  [ { s: "A", t: "B", v: 10.0 }
  , { s: "B", t: "C", v: 10.0 }
  , { s: "C", t: "D", v: 10.0 }
  , { s: "D", t: "A", v: 5.0 }
  , { s: "C", t: "B", v: 3.0 }
  ]

leftAligned :: SankeyConfig
leftAligned = (defaultSankeyConfig 900.0 500.0)
  { alignment = Left
  , nodeWidth = 40.0
  , nodePadding = 14.0
  , extent = { x0: 0.0, y0: 0.0, x1: 900.0, y1: 500.0 }
  }

nodeJson :: SankeyNode -> Json
nodeJson node = fromObject $ FO.fromFoldable
  [ "name" /\ fromString node.name
  , "x0" /\ fromNumber node.x0
  , "y0" /\ fromNumber node.y0
  , "x1" /\ fromNumber node.x1
  , "y1" /\ fromNumber node.y1
  , "value" /\ fromNumber node.value
  , "depth" /\ fromNumber (Int.toNumber node.depth)
  , "nodeHeight" /\ fromNumber (Int.toNumber node.nodeHeight)
  , "layer" /\ fromNumber (Int.toNumber node.layer)
  , "index" /\ nodeIdJson node.index
  , "color" /\ fromString node.color
  ]

nodeIdJson :: NodeID -> Json
nodeIdJson (NodeID i) = fromNumber (Int.toNumber i)

linkJson :: SankeyLink -> Json
linkJson link = fromObject $ FO.fromFoldable
  [ "sourceIndex" /\ nodeIdJson link.sourceIndex
  , "targetIndex" /\ nodeIdJson link.targetIndex
  , "value" /\ fromNumber link.value
  , "width" /\ fromNumber link.width
  , "y0" /\ fromNumber link.y0
  , "y1" /\ fromNumber link.y1
  , "color" /\ fromString link.color
  , "index" /\ (case link.index of LinkID i -> fromNumber (Int.toNumber i))
  ]

resultJson :: SankeyLayoutResult -> Json
resultJson result = fromObject $ FO.fromFoldable
  [ "nodes" /\ fromArray (map nodeJson result.nodes)
  , "links" /\ fromArray (map linkJson result.links)
  ]

lanesJson :: LaneLayout -> Json
lanesJson laid = fromObject $ FO.fromFoldable
  [ "nodes" /\ fromArray (map nodeJson laid.nodes)
  , "waypoints" /\ fromArray (map nodeJson laid.waypoints)
  , "routes" /\ fromArray
      ( laid.routes <#> \route -> fromObject $ FO.fromFoldable
          [ "index" /\ fromNumber (Int.toNumber route.index)
          , "segments" /\ fromArray (map linkJson route.segments)
          ]
      )
  ]

-- | Everything the layout gave, before the change, for inputs the change
-- | must leave alone.
unchanged :: Json
unchanged = fromObject $ FO.fromFoldable
  [ "defaults" /\ resultJson (computeLayoutWithConfig acyclicRows (defaultSankeyConfig 800.0 600.0))
  , "leftAligned" /\ resultJson (computeLayoutWithConfig acyclicRows leftAligned)
  , "lanes" /\ lanesJson (computeLayoutWithLanes (\_ _ -> true) acyclicRows leftAligned)
  , "someLanes" /\ lanesJson (computeLayoutWithLanes (\index _ -> index /= 9) acyclicRows (defaultSankeyConfig 800.0 600.0))
  , "cyclicForwardPart" /\ resultJson (computeLayoutWithConfig cyclicRows leftAligned)
  ]

-- | `cyclicRows`, and: a long link (A to D, three columns), a link from a
-- | node to itself where the node is already known (C), and one where it is
-- | not (E, which nothing else mentions).
tangledRows :: Array LinkCSVRow
tangledRows = cyclicRows <>
  [ { s: "A", t: "D", v: 2.0 }
  , { s: "C", t: "C", v: 2.0 }
  , { s: "E", t: "E", v: 1.5 }
  ]

wholeOptions :: Sizing -> WholeOptions
wholeOptions sizing =
  { sizing
  , backEdges: { margin: 12.0, gap: 6.0, reach: 20.0 }
  , shelf: { margin: 24.0, minimumColumns: 3, columnPitch: 110.0, nodeHeight: 22.0, rowGap: 9.0 }
  }

type Box = { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }

boxOf :: forall r. { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number | r } -> Box
boxOf b = { x0: b.x0, y0: b.y0, x1: b.x1, y1: b.y1 }

overlap :: Box -> Box -> Boolean
overlap a b = a.x0 < b.x1 - 1.0e-9 && b.x0 < a.x1 - 1.0e-9 && a.y0 < b.y1 - 1.0e-9 && b.y0 < a.y1 - 1.0e-9

noneOverlap :: Array Box -> Boolean
noneOverlap boxes = all identity do
  a <- Array.mapWithIndex (\i box -> { i, box }) boxes
  b <- Array.mapWithIndex (\i box -> { i, box }) boxes
  pure (a.i >= b.i || not (overlap a.box b.box))

near :: Number -> Number -> Boolean
near a b = abs (a - b) <= 1.0e-9 * max 1.0 (max (abs a) (abs b))

-- | The bands standing on one side of a node are inside it and clear of one
-- | another.
standsWell :: SankeyNode -> Array { centre :: Number, width :: Number } -> Boolean
standsWell node bands =
  all (\b -> b.centre - b.width / 2.0 >= node.y0 - 1.0e-9 && b.centre + b.width / 2.0 <= node.y1 + 1.0e-9) sorted
    && all identity (Array.zipWith (\a b -> b.centre - b.width / 2.0 >= a.centre + a.width / 2.0 - 1.0e-9) sorted (Array.drop 1 sorted))
  where
  sorted = Array.sortWith _.centre bands

-- | Every back-edge has finite numbers, leaves the right side of the node
-- | its row names as source and enters the left side of the one it names as
-- | target, and on each side of every node the back-edges and the forward
-- | links stand inside the node, clear of one another.
backEdgesStandOnTheirNodes :: Array SankeyNode -> Array SankeyLink -> Array BackEdge -> Boolean
backEdgesStandOnTheirNodes nodes forward edges =
  all finite edges && all attached edges && all sides nodes
  where
  finite e = all isFinite [ e.value, e.width, e.x0, e.y0, e.x1, e.y1 ] && e.width > 0.0
  named name = Array.find (\n -> n.name == name) nodes
  attached e = case named e.source, named e.target of
    Just source, Just target -> e.x0 == source.x1 && e.x1 == target.x0
    _, _ -> false
  sides node =
    standsWell node
      ( (Array.filter (\l -> l.sourceIndex == node.index) forward <#> \l -> { centre: l.y0, width: l.width })
          <> (Array.filter (\e -> e.source == node.name) edges <#> \e -> { centre: e.y0, width: e.width })
      )
      && standsWell node
        ( (Array.filter (\l -> l.targetIndex == node.index) forward <#> \l -> { centre: l.y1, width: l.width })
            <> (Array.filter (\e -> e.target == node.name) edges <#> \e -> { centre: e.y1, width: e.width })
        )

sankeyWholeTestCount :: Int
sankeyWholeTestCount = 9

runSankeyWholeTests :: Effect Int
runSankeyWholeTests = do
  log "\n=== Sankey: self-links, isolated nodes, back-edges ==="

  golden <- assertGolden "sankey-unchanged-full-precision.golden.json" (stringify unchanged)
  unchangedFailure <- report "inputs without self-links, isolated nodes or back-edges are laid out as before" (passed golden)

  -- A link from a node to itself
  let
    alone = computeLayoutWithConfig [ { s: "again", t: "again", v: 2.0 } ] leftAligned
    first = computeLayoutWithConfig [ { s: "A", t: "A", v: 1.0 }, { s: "A", t: "B", v: 3.0 } ] leftAligned
    later = computeLayoutWithConfig [ { s: "A", t: "B", v: 3.0 }, { s: "A", t: "A", v: 1.0 } ] leftAligned
    withoutIt = computeLayoutWithConfig [ { s: "A", t: "B", v: 3.0 } ] leftAligned
    layers result = result.nodes <#> \n -> { name: n.name, layer: n.layer }
  selfLinkFailure <- report "a link from a node to itself gives one node, whether or not the node is already known"
    ( map _.name alone.nodes == [ "again" ]
        && map _.name first.nodes == [ "A", "B" ]
        && map _.name later.nodes == [ "A", "B" ]
    )
  selfLinkMeaningFailure <- report "it is a back-edge: no forward link, no change of column, and its node is tall enough for it on both sides"
    ( Array.null alone.links
        && (backEdges alone <#> \e -> { index: e.index, source: e.source, target: e.target }) == [ { index: 0, source: "again", target: "again" } ]
        && (backEdges first <#> \e -> { index: e.index, source: e.source, target: e.target }) == [ { index: 0, source: "A", target: "A" } ]
        && layers first == layers withoutIt
        && layers later == layers withoutIt
        && map _.value first.nodes == [ 4.0, 3.0 ]
        && all (\result -> backEdgesStandOnTheirNodes result.nodes result.links (backEdges result)) [ alone, first, later ]
    )

  -- Back-edges
  let
    tangled = computeLayoutWithLanes (\_ _ -> true) tangledRows leftAligned
    plainTangled = computeLayoutWithConfig tangledRows leftAligned
    identities edges = edges <#> \e -> { index: e.index, source: e.source, target: e.target }
    expected =
      [ { index: 3, source: "D", target: "A" }
      , { index: 4, source: "C", target: "B" }
      , { index: 6, source: "C", target: "C" }
      , { index: 7, source: "E", target: "E" }
      ]
    forwardOf laid = Array.concatMap _.segments laid.routes
    routed = routeUnder { top: 520.0, gap: 6.0 } tangled.backEdges
    paths = map (generateBackEdgePath 20.0) routed.routed
  identityFailure <- report "back-edges are told by their place in the input and their rows' names, with and without lanes"
    (identities tangled.backEdges == expected && identities (backEdges plainTangled) == expected && Array.length tangled.waypoints == 2)
  geometryFailure <- report "every back-edge has finite geometry, standing on its two nodes clear of their other links"
    ( backEdgesStandOnTheirNodes (tangled.nodes <> tangled.waypoints) (forwardOf tangled) tangled.backEdges
        && backEdgesStandOnTheirNodes plainTangled.nodes plainTangled.links (backEdges plainTangled)
        && all (\route -> Array.null route.segments == Array.elem route.index (map _.index expected)) tangled.routes
    )
  routeFailure <- report "routed under the chart: one channel each, clear of one another, and a path with no NaN in it"
    ( Array.length paths == 4
        && all (\path -> String.length path > 0 && not (contains (Pattern "NaN") path) && not (contains (Pattern "Infinity") path)) paths
        && all identity (Array.zipWith (\a b -> b.channel - b.backEdge.width / 2.0 >= a.channel + a.backEdge.width / 2.0 + 6.0 - 1.0e-9) routed.routed (Array.drop 1 routed.routed))
        && map (\r -> r.channel - r.backEdge.width / 2.0) (Array.head routed.routed) == Just 520.0
        && map (\r -> near routed.bottom (r.channel + r.backEdge.width / 2.0)) (Array.last routed.routed) == Just true
    )

  -- Isolated nodes
  let
    lone = [ "p", "A", "q", "r", "E", "s", "t", "q", "u", "v" ]
    whole = computeWholeLayout (wholeOptions FitExtent) (\_ _ -> true) lone tangledRows leftAligned
    none = computeWholeLayout (wholeOptions FitExtent) (\_ _ -> true) [] acyclicRows leftAligned
    onlyLone = computeWholeLayout (wholeOptions FitExtent) (\_ _ -> true) lone [] leftAligned
    everyBox laid = map boxOf laid.nodes <> map boxOf laid.waypoints <> map boxOf laid.isolated
    insideExtent laid = all (\b -> b.x0 >= laid.extent.x0 && b.y0 >= laid.extent.y0 && b.x1 <= laid.extent.x1 && b.y1 <= laid.extent.y1) (everyBox laid)
  isolatedFailure <- report "nodes no link mentions are all placed, once each, in order, clear of each other, of the chart and of the channels"
    ( isolatedAmong lone tangledRows == [ "p", "q", "r", "s", "t", "u", "v" ]
        && map _.name whole.isolated == [ "p", "q", "r", "s", "t", "u", "v" ]
        && map _.name onlyLone.isolated == [ "p", "A", "q", "r", "E", "s", "t", "u", "v" ]
        && all (\b -> all isFinite [ b.x0, b.y0, b.x1, b.y1 ] && b.x1 > b.x0 && b.y1 > b.y0) (map boxOf whole.isolated <> map boxOf onlyLone.isolated)
        && noneOverlap (everyBox whole)
        && noneOverlap (everyBox onlyLone)
        && insideExtent whole
        && insideExtent onlyLone
        && all (\n -> all (\r -> n.y0 >= r.channel + r.backEdge.width / 2.0) whole.backEdges) whole.isolated
        && Array.length whole.backEdges == 4
    )
  noneFailure <- report "with no isolated node and no fixed scale, the whole layout is the lanes layout"
    ( stringify (lanesJson { nodes: none.nodes, waypoints: none.waypoints, routes: none.routes, cycleAnalysis: none.cycleAnalysis, backEdges: [] })
        == stringify (lanesJson (computeLayoutWithLanes (\_ _ -> true) acyclicRows leftAligned))
        && Array.null none.isolated
        && Array.null none.backEdges
    )

  -- A fixed scale
  let
    scale = { pixelsPerUnit: 7.0, columnPitch: 130.0 }
    scaled = computeLayoutWithLanesAtScale scale (\_ _ -> true) tangledRows leftAligned
    plainScaled = computeLayoutWithConfig acyclicRows (configAtScale scale acyclicRows leftAligned)
    columnLeft nodes layer = _.x0 <$> Array.find (\n -> n.layer == layer) nodes
    atScale links = all (\l -> near l.width (7.0 * l.value)) links
    tall nodes = all (\n -> near (n.y1 - n.y0) (7.0 * n.value)) nodes
  scaleFailure <- report "at a fixed scale a unit of value is that many pixels and the columns are that far apart"
    ( atScale (forwardOf scaled) && all (\e -> near e.width (7.0 * e.value)) scaled.backEdges && tall (scaled.nodes <> scaled.waypoints)
        && atScale plainScaled.links && tall plainScaled.nodes
        && map2 (\a b -> near (b - a) 130.0) (columnLeft scaled.nodes 0) (columnLeft scaled.nodes 1) == Just true
        && map2 (\a b -> near (b - a) 390.0) (columnLeft plainScaled.nodes 0) (columnLeft plainScaled.nodes 3) == Just true
        && noneOverlap (map boxOf (scaled.nodes <> scaled.waypoints))
    )

  let
    failures = Array.length $ Array.filter identity
      [ unchangedFailure, selfLinkFailure, selfLinkMeaningFailure, identityFailure, geometryFailure, routeFailure, isolatedFailure, noneFailure, scaleFailure ]
  log $ "\nSankey whole tests: " <> show (sankeyWholeTestCount - failures) <> "/" <> show sankeyWholeTestCount <> " passed"
  pure failures
  where
  map2 f a b = f <$> a <*> b
  passed = case _ of
    GoldenMismatch _ _ -> false
    GoldenMatch -> true
    GoldenCreated -> true

-- | Logs the outcome; `true` is a failure.
report :: String -> Boolean -> Effect Boolean
report name ok = do
  log $ (if ok then "  PASS: " else "  FAIL: ") <> name
  pure (not ok)
