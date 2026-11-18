-- | Test.Layout.PlacedSpec
-- |
-- | `DataViz.Layout.Hierarchy.Placed` adds no layout of its own, so there is
-- | nothing to hold to d3 here: each projection is held to the layout module
-- | it calls, node for node and exactly, and the projections to each other on
-- | what is true of a node in every layout.
module Test.Layout.PlacedSpec
  ( runPlacedTests
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, sum)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tree as Rose
import Data.Tuple (Tuple(..))
import DataViz.Layout.Hierarchy.Cluster as Cluster
import DataViz.Layout.Hierarchy.Core as Core
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import DataViz.Layout.Hierarchy.Placed (Extent(..), Hierarchy, Placement)
import DataViz.Layout.Hierarchy.Placed as Placed
import DataViz.Layout.Hierarchy.Tree as Tree
import DataViz.Layout.Hierarchy.Treemap as Treemap
import Effect (Effect)
import Effect.Console (log)

newtype Item = Item { name :: String, size :: Number, parts :: Array Item }

leaf :: String -> Number -> Item
leaf name ownSize = Item { name, size: ownSize, parts: [] }

branch :: String -> Array Item -> Item
branch name parts = Item { name, size: 0.0, parts }

-- | Fourteen nodes, leaves at three depths, a parent with one child, and
-- | sizes in no particular order (Pack sorts its children by size).
example :: Item
example =
  branch "root"
    [ branch "a" [ leaf "a1" 30.0, leaf "a2" 5.0, branch "a3" [ leaf "a3x" 12.0, leaf "a3y" 44.0 ] ]
    , leaf "b" 25.0
    , branch "c" [ branch "c1" [ leaf "c1x" 9.0 ] ]
    , branch "d" [ leaf "d1" 3.0, leaf "d2" 70.0, leaf "d3" 18.0 ]
    ]

hierarchy :: Hierarchy Item String
hierarchy =
  { root: example
  , children: \(Item item) -> item.parts
  , value: \(Item item) -> item.size
  , key: \(Item item) -> item.name
  }

names :: Item -> Array String
names (Item item) = [ item.name ] <> Array.concatMap names item.parts

size :: { width :: Number, height :: Number }
size = { width: 400.0, height: 300.0 }

type Check = { name :: String, passed :: Boolean }

-- | Every node has a placement, and nothing else does.
complete :: Map String Placement -> Boolean
complete placed = Array.sort (Array.fromFoldable (Map.keys placed)) == Array.sort (names example)

-- | What is true of a node whatever the layout.
facts :: Map String Placement -> Map String { depth :: Int, height :: Int, value :: Number, leaf :: Boolean }
facts = map \placement -> { depth: placement.depth, height: placement.height, value: placement.value, leaf: placement.leaf }

checks :: Array Check
checks =
  [ { name: "every layout places every node once", passed: all complete [ treed, clustered, packed, squared, parted ] }
  , { name: "depth, height, value and leaf agree across the five layouts"
    , passed: all (\placed -> facts placed == facts treed) [ clustered, packed, squared, parted ]
    }
  , { name: "depth, height and value are right"
    , passed:
        (Map.lookup "root" (facts treed) == Just { depth: 0, height: 3, value: 216.0, leaf: false })
          && (Map.lookup "a3y" (facts treed) == Just { depth: 3, height: 0, value: 44.0, leaf: true })
          && (Map.lookup "c" (facts treed) == Just { depth: 1, height: 2, value: 9.0, leaf: false })
    }
  , { name: "tree is Tree.tree, node for node"
    , passed: (treed <#> \p -> Tuple p.x p.y) == direct (Tree.tree (Tree.defaultTreeConfig { size = size }) (rose example))
        && all (\p -> p.extent == Dot) treed
    }
  , { name: "cluster is Cluster.cluster, node for node"
    , passed: (clustered <#> \p -> Tuple p.x p.y) == direct (Cluster.cluster (Cluster.defaultClusterConfig { size = size }) (rose example))
    }
  , { name: "pack is Pack.pack, node for node"
    , passed: (packed <#> \p -> { x: p.x, y: p.y, extent: p.extent }) == Map.fromFoldable (circles directPack)
    }
  , { name: "treemap is Treemap.treemap, node for node"
    , passed: (squared <#> _.extent) == Map.fromFoldable (cells directTreemap)
    }
  , { name: "partition is Partition.partition, node for node"
    , passed: (parted <#> _.extent) == Map.fromFoldable (bands directPartition)
    }
  , { name: "partition rounded to pixels is d3-hierarchy 3.1.2's (padding 1, round on)"
    , passed: (Placed.partition (Partition.defaultPartitionConfig { size = size, padding = 1.0, round = true }) hierarchy <#> _.extent)
        == d3Rounded
    }
  , { name: "a rectangle's point is its centre"
    , passed: all centred squared && all centred parted
    }
  , { name: "a treemap's leaves share out the area by value"
    , passed: (sum (Map.filter _.leaf squared <#> area) - size.width * size.height) < 1.0e-6
        && (sum (Map.filter _.leaf squared <#> area) - size.width * size.height) > -1.0e-6
    }
  ]
  where
  treed = Placed.tree (Tree.defaultTreeConfig { size = size }) hierarchy
  clustered = Placed.cluster (Cluster.defaultClusterConfig { size = size }) hierarchy
  packed = Placed.pack (Pack.defaultPackConfig { size = size, padding = 2.0 }) hierarchy
  squared = Placed.treemap (Treemap.defaultTreemapConfig { size = size }) hierarchy
  parted = Placed.partition (Partition.defaultPartitionConfig { size = size, padding = 1.0 }) hierarchy

  rose (Item item) =
    Rose.mkTree { name: item.name, x: 0.0, y: 0.0, depth: 0, height: 0 } (List.fromFoldable (rose <$> item.parts))
  direct laidOut = Map.fromFoldable (Array.fromFoldable laidOut <#> \node -> Tuple node.name (Tuple node.x node.y))

  packSource (Item item) = Pack.HierarchyData
    { data_: item.name
    , value: if Array.null item.parts then Just item.size else Nothing
    , children: if Array.null item.parts then Nothing else Just (packSource <$> item.parts)
    }
  directPack = Pack.pack (Pack.defaultPackConfig { size = size, padding = 2.0 }) (Pack.hierarchy (packSource example))
  circles (Pack.PackNode node) =
    [ Tuple node.data_ { x: node.x, y: node.y, extent: Circle { radius: node.r } } ] <> Array.concatMap circles node.children

  parts (Item item) = if Array.null item.parts then Nothing else Just item.parts
  ownSize (Item item) = if Array.null item.parts then item.size else 0.0
  directTreemap = Treemap.treemap (Treemap.defaultTreemapConfig { size = size }) (Core.sum (Core.hierarchy example parts) ownSize)
  cells (Treemap.TNode node) =
    let
      Item item = node.data_
    in
      [ Tuple item.name (Rectangle { x0: node.x0, y0: node.y0, x1: node.x1, y1: node.y1 }) ] <> Array.concatMap cells node.children

  partitionSource (Item item) = Partition.HierarchyData
    { data_: item.name
    , value: if Array.null item.parts then Just item.size else Nothing
    , children: if Array.null item.parts then Nothing else Just (partitionSource <$> item.parts)
    }
  directPartition = Partition.partition (Partition.defaultPartitionConfig { size = size, padding = 1.0 })
    (Partition.hierarchy (partitionSource example))
  bands (Partition.PartNode node) =
    [ Tuple node.data_ (Rectangle { x0: node.x0, y0: node.y0, x1: node.x1, y1: node.y1 }) ] <> Array.concatMap bands node.children

  centred placement = case placement.extent of
    Rectangle r -> placement.x == (r.x0 + r.x1) / 2.0 && placement.y == (r.y0 + r.y1) / 2.0
    _ -> false
  area placement = case placement.extent of
    Rectangle r -> (r.x1 - r.x0) * (r.y1 - r.y0)
    _ -> 0.0

-- | `d3.partition().size([400, 300]).padding(1).round(true)` on the example,
-- | values summed from the leaves: d3-hierarchy 3.1.2's own output.
d3Rounded :: Map String Extent
d3Rounded = Map.fromFoldable
    [ Tuple "root" (Rectangle { x0: 1.0, y0: 1.0, x1: 399.0, y1: 74.0 })
    , Tuple "a" (Rectangle { x0: 1.0, y0: 75.0, x1: 168.0, y1: 149.0 })
    , Tuple "b" (Rectangle { x0: 169.0, y0: 75.0, x1: 214.0, y1: 149.0 })
    , Tuple "c" (Rectangle { x0: 215.0, y0: 75.0, x1: 231.0, y1: 149.0 })
    , Tuple "d" (Rectangle { x0: 232.0, y0: 75.0, x1: 399.0, y1: 149.0 })
    , Tuple "a1" (Rectangle { x0: 1.0, y0: 150.0, x1: 55.0, y1: 224.0 })
    , Tuple "a2" (Rectangle { x0: 56.0, y0: 150.0, x1: 65.0, y1: 224.0 })
    , Tuple "a3" (Rectangle { x0: 66.0, y0: 150.0, x1: 168.0, y1: 224.0 })
    , Tuple "c1" (Rectangle { x0: 215.0, y0: 150.0, x1: 231.0, y1: 224.0 })
    , Tuple "d1" (Rectangle { x0: 232.0, y0: 150.0, x1: 236.0, y1: 224.0 })
    , Tuple "d2" (Rectangle { x0: 237.0, y0: 150.0, x1: 366.0, y1: 224.0 })
    , Tuple "d3" (Rectangle { x0: 367.0, y0: 150.0, x1: 399.0, y1: 224.0 })
    , Tuple "a3x" (Rectangle { x0: 66.0, y0: 225.0, x1: 87.0, y1: 299.0 })
    , Tuple "a3y" (Rectangle { x0: 88.0, y0: 225.0, x1: 168.0, y1: 299.0 })
    , Tuple "c1x" (Rectangle { x0: 215.0, y0: 225.0, x1: 231.0, y1: 299.0 })
    ]

-- | Run the checks; the number that failed.
runPlacedTests :: Effect Int
runPlacedTests = do
  log "\n--- Placed (one hierarchy, five layouts) ---"
  failures <- traverse report checks
  pure (sum failures)
  where
  report check = do
    log ((if check.passed then "  PASS: " else "  FAIL: ") <> check.name)
    pure (if check.passed then 0 else 1)
