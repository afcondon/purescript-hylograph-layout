-- | DataViz.Layout.TidyDag
-- |
-- | Layout for shallow, sparse DAGs (build graphs, dependency cones):
-- | prune the DAG to a spanning tree, lay the tree out with the
-- | Reingold–Tilford `Hierarchy.Tree`, then hand back the pruned edges
-- | as cross-links for the renderer to draw faint.
-- |
-- | The pruning heuristic: each node's tree-parent is its
-- | LONGEST-PATH parent — the dependent that determines the node's
-- | layer — so the tree backbone agrees with the DAG's natural stages
-- | and cross-links stay short and lateral. Ties break by the caller's
-- | `mass` (e.g. transitive leaf mass), then lexically for determinism.
-- |
-- | Edges are `parent` depends-on `child` (build: target → input), so
-- | roots are the goals and leaves are the sources. Self-edges are
-- | ignored; cycles cannot capture the depth pass (it is a bounded
-- | fixpoint), and back-edges simply become cross-links.
module DataViz.Layout.TidyDag
  ( TidyDagConfig
  , defaultTidyDagConfig
  , DagEdge
  , LinkKind(..)
  , PlacedNode
  , PlacedLink
  , TidyDagLayout
  , tidyDag
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tree (Tree, mkTree)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Hierarchy.Tree (defaultTreeConfig, tree)

type TidyDagConfig =
  { size :: { width :: Number, height :: Number }
  , minSeparation :: Number
  , layerSeparation :: Maybe Number
  }

defaultTidyDagConfig :: TidyDagConfig
defaultTidyDagConfig =
  { size: { width: 900.0, height: 560.0 }
  , minSeparation: 2.0
  , layerSeparation: Nothing
  }

-- | `parent` depends on `child` (a build target and one of its inputs).
type DagEdge = { parent :: String, child :: String }

data LinkKind = TreeLink | CrossLink

derive instance Eq LinkKind

instance Show LinkKind where
  show = case _ of
    TreeLink -> "tree"
    CrossLink -> "cross"

type PlacedNode = { id :: String, x :: Number, y :: Number, depth :: Int }

type PlacedLink = { parent :: String, child :: String, kind :: LinkKind }

type TidyDagLayout = { nodes :: Array PlacedNode, links :: Array PlacedLink }

type NodeRec = { id :: String, x :: Number, y :: Number, depth :: Int }

-- | Lay out a DAG. `roots` are the entry points (goals); ids appearing
-- | only in `edges` are picked up automatically. `mass` breaks
-- | tree-parent ties (bigger wins); `const 1.0` is fine.
tidyDag
  :: TidyDagConfig
  -> { edges :: Array DagEdge
     , roots :: Array String
     , mass :: String -> Number
     }
  -> TidyDagLayout
tidyDag config { edges, roots, mass } =
  { nodes: placed
  , links: cleanEdges <#> \e ->
      { parent: e.parent
      , child: e.child
      , kind:
          if Map.lookup e.child treeParent == Just e.parent then TreeLink
          else CrossLink
      }
  }
  where
  cleanEdges = Array.nub (Array.filter (\e -> e.parent /= e.child) edges)

  allIds = Array.nub
    (roots <> (cleanEdges <#> _.parent) <> (cleanEdges <#> _.child))

  -- tree roots are the nodes NOTHING depends on (a goal that is also
  -- some other node's input hangs under that dependent, or it would
  -- be planted twice); the `roots` argument only guarantees isolated
  -- goals join `allIds` at all
  childSet = Set.fromFoldable (cleanEdges <#> _.child)
  effectiveRoots = Array.filter (\i -> not (Set.member i childSet)) allIds

  -- longest-path depth from the roots: bounded fixpoint over the edges
  -- (|V| passes suffice for a DAG; cycles just stop improving)
  depths :: Map String Int
  depths = go (Map.fromFoldable (effectiveRoots <#> \r -> Tuple r 0)) (Array.length allIds)
    where
    go acc 0 = acc
    go acc n =
      let
        acc' = Array.foldl step acc cleanEdges
        step m e = case Map.lookup e.parent m of
          Nothing -> m
          Just dp ->
            let candidate = dp + 1
            in case Map.lookup e.child m of
              Just dc | dc >= candidate -> m
              _ -> Map.insert e.child candidate m
      in
        if acc' == acc then acc else go acc' (n - 1)

  depthOf i = fromMaybe 0 (Map.lookup i depths)

  -- the longest-path parent: a dependent one layer up; ties by mass,
  -- then lexically (Array.sortWith is stable, ids are nubbed)
  treeParent :: Map String String
  treeParent = Map.fromFoldable $ Array.mapMaybe pick groupedByChild
    where
    groupedByChild = Array.groupAllBy (\a b -> compare a.child b.child) cleanEdges
      <#> \grp ->
        { child: (NEA.head grp).child
        , parents: NEA.toArray grp <#> _.parent
        }

    pick { child, parents } =
      Array.head
        ( Array.sortBy
            ( \a b ->
                compare (dist b) (dist a) -- deepest parent first
                  <> compare (mass b) (mass a) -- then heaviest
                  <> compare a b -- then lexical
            )
            parents
        ) <#> \p -> Tuple child p
      where
      dist p = depthOf p

  childrenOf :: String -> Array String
  childrenOf i =
    Array.sortBy compare
      (Map.toUnfoldable treeParent # Array.mapMaybe \(Tuple c p) -> if p == i then Just c else Nothing)

  buildTree :: String -> Tree NodeRec
  buildTree i = mkTree
    { id: i, x: 0.0, y: 0.0, depth: depthOf i }
    (List.fromFoldable (buildTree <$> childrenOf i))

  virtualRootId :: String
  virtualRootId = "\x00tidydag-virtual-root"

  laidOut :: Tree NodeRec
  laidOut = case effectiveRoots of
    [ r ] -> tree treeConfig (buildTree r)
    rs -> tree treeConfig $ mkTree
      { id: virtualRootId, x: 0.0, y: 0.0, depth: -1 }
      (List.fromFoldable (buildTree <$> rs))

  treeConfig = defaultTreeConfig
    { size = config.size
    , minSeparation = config.minSeparation
    , layerSeparation = config.layerSeparation
    }

  placed :: Array PlacedNode
  placed = Array.filter (\n -> n.id /= virtualRootId) (flatten laidOut)

  flatten :: Tree NodeRec -> Array PlacedNode
  flatten t =
    let
      n = head t
      rest :: List (Tree NodeRec)
      rest = tail t
    in
      [ { id: n.id, x: n.x, y: n.y, depth: n.depth } ]
        <> Array.concatMap flatten (Array.fromFoldable rest)
