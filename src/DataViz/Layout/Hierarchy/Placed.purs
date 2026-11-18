-- | DataViz.Layout.Hierarchy.Placed
-- |
-- | The five hierarchy layouts behind one door: describe the hierarchy once,
-- | and get, from any of the layouts, a `Map` from each node's key to where
-- | that layout put it.
-- |
-- | Each layout module has its own input type (a `Data.Tree` of records for
-- | Tree and Cluster, `Pack.HierarchyData`, `Partition.HierarchyData`, a
-- | `ValuedNode` for Treemap) and its own output tree. That is right for a
-- | caller that draws one layout. A caller that wants several layouts of the
-- | same data, to compare them or to move between them, would otherwise write
-- | a converter in and a flattener out for each.
-- |
-- | ```purescript
-- | flare :: Hierarchy Node String
-- | flare = { root, children: _.children, value: _.size, key: _.path }
-- |
-- | packed  = Placed.pack (Pack.defaultPackConfig { size = size }) flare
-- | squared = Placed.treemap (Treemap.defaultTreemapConfig { size = size }) flare
-- | -- Map.lookup "flare.vis.Legend" packed :: Maybe Placement
-- | ```
-- |
-- | Nothing is computed differently: each function calls the layout module's
-- | own function with the configuration given, and reads the result off.
module DataViz.Layout.Hierarchy.Placed
  ( Hierarchy
  , Placement
  , Extent(..)
  , TreeNode
  , Node
  , tree
  , cluster
  , pack
  , treemap
  , partition
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (maximum, sum)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tree as Rose
import Data.Tuple (Tuple(..))
import DataViz.Layout.Hierarchy.Cluster as Cluster
import DataViz.Layout.Hierarchy.Core as Core
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import DataViz.Layout.Hierarchy.Tree as Tree
import DataViz.Layout.Hierarchy.Treemap as Treemap

-- | A hierarchy, described by how to walk it. `children` is empty for a leaf.
-- | `value` is asked of leaves only (an internal node's value is the sum of
-- | its leaves'); a hierarchy with no natural sizes can say `const 1.0`.
-- | `key` must be different for every node.
type Hierarchy node key =
  { root :: node
  , children :: node -> Array node
  , value :: node -> Number
  , key :: node -> key
  }

-- | The room a layout gave a node, beyond a point.
data Extent
  = Dot -- Tree, Cluster: a node is a point
  | Circle { radius :: Number } -- Pack
  | Rectangle { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number } -- Treemap, Partition

derive instance Eq Extent

-- | Where a layout put a node. (`x`, `y`) is the node's point for Tree and
-- | Cluster, the centre of its circle for Pack, and the centre of its
-- | rectangle for Treemap and Partition. `depth` counts from the root (0),
-- | `height` from the deepest leaf below (0 for a leaf), and `value` is the
-- | sum of the values of the leaves at or below the node. These three are
-- | the same in every layout of the same hierarchy.
type Placement =
  { x :: Number
  , y :: Number
  , extent :: Extent
  , depth :: Int
  , height :: Int
  , value :: Number
  , leaf :: Boolean
  }

-- | The hierarchy as every layout here is given it: keys, with what is true
-- | of a node whatever the layout.
newtype Node key = Node
  { key :: key
  , depth :: Int
  , height :: Int
  , value :: Number
  , children :: Array (Node key)
  }

annotate :: forall node key. Hierarchy node key -> Node key
annotate hierarchy = go 0 hierarchy.root
  where
  go depth node =
    let
      children = go (depth + 1) <$> hierarchy.children node
      leaf = Array.null children
    in
      Node
        { key: hierarchy.key node
        , depth
        , height: if leaf then 0 else 1 + fromMaybe 0 (maximum (children <#> \(Node child) -> child.height))
        , value: if leaf then hierarchy.value node else sum (children <#> \(Node child) -> child.value)
        , children
        }

isLeaf :: forall key. Node key -> Boolean
isLeaf (Node node) = Array.null node.children

-- | The record the Tree and Cluster layouts work on, and so the type their
-- | configuration's `separation` function is given.
type TreeNode key =
  { key :: key
  , x :: Number
  , y :: Number
  , depth :: Int
  , height :: Int
  , value :: Number
  , leaf :: Boolean
  }

rose :: forall key. Node key -> Rose.Tree (TreeNode key)
rose node@(Node inner) =
  Rose.mkTree
    { key: inner.key, x: 0.0, y: 0.0, depth: inner.depth, height: inner.height, value: inner.value, leaf: isLeaf node }
    (List.fromFoldable (rose <$> inner.children))

points :: forall key. Ord key => Rose.Tree (TreeNode key) -> Map key Placement
points laidOut = Map.fromFoldable $ Array.fromFoldable laidOut <#> \node ->
  Tuple node.key
    { x: node.x, y: node.y, extent: Dot, depth: node.depth, height: node.height, value: node.value, leaf: node.leaf }

-- | `DataViz.Layout.Hierarchy.Tree.tree`: x across the configured width, y
-- | down the configured height by depth.
tree :: forall node key. Ord key => Tree.TreeConfig (TreeNode key) -> Hierarchy node key -> Map key Placement
tree config = points <<< Tree.tree config <<< rose <<< annotate

-- | `DataViz.Layout.Hierarchy.Cluster.cluster`: as `tree`, with every leaf at
-- | the full height.
cluster :: forall node key. Ord key => Cluster.ClusterConfig -> Hierarchy node key -> Map key Placement
cluster config = points <<< Cluster.cluster config <<< rose <<< annotate

-- | `DataViz.Layout.Hierarchy.Pack.pack`. The configuration's `radius`
-- | function, if any, is given a node's key.
pack :: forall node key. Ord key => Pack.PackConfig key -> Hierarchy node key -> Map key Placement
pack config hierarchy = Map.fromFoldable (flatten (Pack.pack config (Pack.hierarchy (source root))))
  where
  root = annotate hierarchy

  source node@(Node inner) = Pack.HierarchyData
    { data_: inner.key
    , value: if isLeaf node then Just inner.value else Nothing
    , children: if isLeaf node then Nothing else Just (source <$> inner.children)
    }

  -- Pack sorts children by value, so its tree is not in the order of ours:
  -- what is true of a node in every layout is looked up by key.
  known = Map.fromFoldable (everyNode root <#> \node@(Node inner) -> Tuple inner.key node)

  flatten (Pack.PackNode placed) =
    let
      Node inner = fromMaybe (Node { key: placed.data_, depth: placed.depth, height: placed.height, value: placed.value, children: [] })
        (Map.lookup placed.data_ known)
    in
      [ Tuple placed.data_
          { x: placed.x
          , y: placed.y
          , extent: Circle { radius: placed.r }
          , depth: inner.depth
          , height: inner.height
          , value: inner.value
          , leaf: Array.null placed.children
          }
      ] <> Array.concatMap flatten placed.children

everyNode :: forall key. Node key -> Array (Node key)
everyNode node@(Node inner) = [ node ] <> Array.concatMap everyNode inner.children

-- | `DataViz.Layout.Hierarchy.Treemap.treemap`.
treemap :: forall node key. Ord key => Treemap.TreemapConfig (Node key) -> Hierarchy node key -> Map key Placement
treemap config hierarchy = Map.fromFoldable (flatten (Treemap.treemap config valued))
  where
  children node@(Node inner) = if isLeaf node then Nothing else Just inner.children
  ownValue node@(Node inner) = if isLeaf node then inner.value else 0.0
  valued = Core.sum (Core.hierarchy (annotate hierarchy) children) ownValue

  flatten (Treemap.TNode placed) =
    let
      node@(Node inner) = placed.data_
    in
      [ Tuple inner.key
          { x: (placed.x0 + placed.x1) / 2.0
          , y: (placed.y0 + placed.y1) / 2.0
          , extent: Rectangle { x0: placed.x0, y0: placed.y0, x1: placed.x1, y1: placed.y1 }
          , depth: inner.depth
          , height: inner.height
          , value: inner.value
          , leaf: isLeaf node
          }
      ] <> Array.concatMap flatten placed.children

-- | `DataViz.Layout.Hierarchy.Partition.partition`: x along the configured
-- | width by value, y down the configured height by depth. (For a sunburst,
-- | read x as an angle and y as a radius.)
partition :: forall node key. Ord key => Partition.PartitionConfig key -> Hierarchy node key -> Map key Placement
partition config hierarchy = Map.fromFoldable (flatten (Partition.partition config (Partition.hierarchy (source root))))
  where
  root = annotate hierarchy

  source node@(Node inner) = Partition.HierarchyData
    { data_: inner.key
    , value: if isLeaf node then Just inner.value else Nothing
    , children: if isLeaf node then Nothing else Just (source <$> inner.children)
    }

  known = Map.fromFoldable (everyNode root <#> \node@(Node inner) -> Tuple inner.key node)

  flatten (Partition.PartNode placed) =
    let
      Node inner = fromMaybe (Node { key: placed.data_, depth: placed.depth, height: placed.height, value: placed.value, children: [] })
        (Map.lookup placed.data_ known)
    in
      [ Tuple placed.data_
          { x: (placed.x0 + placed.x1) / 2.0
          , y: (placed.y0 + placed.y1) / 2.0
          , extent: Rectangle { x0: placed.x0, y0: placed.y0, x1: placed.x1, y1: placed.y1 }
          , depth: inner.depth
          , height: inner.height
          , value: inner.value
          , leaf: Array.null placed.children
          }
      ] <> Array.concatMap flatten placed.children
