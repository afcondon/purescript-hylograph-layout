# Extract DFS back-edge detection to hylograph-graph

## Context

The `purescript-hylograph-layout` library contains a DFS-based back-edge detection algorithm (`findBackEdgePairs`) that should be promoted to the `hylograph-graph` library (`Data.Graph.Algorithms`). This algorithm was written because `findCycle` in the graph library has a bug where it returns phantom cycles that don't correspond to actual edges (see separate bug fix work on that).

The algorithm is more useful than `findCycle` for layout engines: it classifies ALL back-edges in a single DFS pass, rather than finding one cycle at a time.

## Source code location

**File:** `purescript-hylograph-layout/src/DataViz/Layout/Sankey/Compute.purs`

**Function:** `findBackEdgePairs` (around line 175)

```purescript
-- | Find all back-edge pairs using DFS three-color marking.
-- | An edge (u, v) is a back-edge if v is gray (in the current DFS stack) when visited from u.
findBackEdgePairs :: WG.WeightedDigraph NodeID Number -> Set.Set (Tuple NodeID NodeID)
findBackEdgePairs graph =
  let
    allNodes = WG.nodes graph
    initial = { white: Set.fromFoldable allNodes, gray: Set.empty, black: Set.empty, backEdges: Set.empty }
    result = foldl (\state node ->
      if Set.member node state.white then dfsClassify state node
      else state
    ) initial allNodes
  in result.backEdges
  where
  dfsClassify state node =
    let
      state' = state
        { white = Set.delete node state.white
        , gray = Set.insert node state.gray
        }
      neighbors = WG.outgoing node graph
      state'' = foldl (\st edge ->
        if Set.member edge.target st.gray then
          st { backEdges = Set.insert (Tuple node edge.target) st.backEdges }
        else if Set.member edge.target st.white then
          dfsClassify st edge.target
        else st
      ) state' neighbors
    in
      state'' { gray = Set.delete node state''.gray, black = Set.insert node state''.black }
```

## What to do

### 1. Generalize the type signature

The current version is specialized to `WeightedDigraph NodeID Number`. The library version should work on `SimpleGraph node` (consistent with other algorithms in `Data.Graph.Algorithms`):

```purescript
-- | Find all back-edges in a directed graph using DFS three-color marking.
-- | A back-edge (u, v) exists when v is an ancestor of u in the DFS tree
-- | (i.e., v is gray/in-stack when visited from u).
-- | Returns the set of (source, target) pairs that are back-edges.
-- | Removing all back-edges from a graph makes it acyclic (feedback arc set).
findBackEdges :: forall node. Ord node => SimpleGraph node -> Set (Tuple node node)
```

### 2. Optionally add full edge classification

A more general version would classify ALL edges, not just back-edges:

```purescript
data EdgeClass = TreeEdge | BackEdge | ForwardEdge | CrossEdge

classifyEdges :: forall node. Ord node => SimpleGraph node -> Map (Tuple node node) EdgeClass
```

This would be valuable for general graph analysis. The DFS state needs to track discovery times to distinguish forward from cross edges, but the three-color approach already handles tree vs back correctly.

### 3. Add a convenience function for WeightedDigraph

```purescript
-- | Find back-edges in a weighted digraph.
-- | Convenience wrapper that converts to SimpleGraph internally.
findBackEdgesWeighted :: forall node weight. Ord node => WeightedDigraph node weight -> Set (Tuple node node)
findBackEdgesWeighted g = findBackEdges (toSimpleGraph g)
```

### 4. Integration with existing API

The new functions should sit alongside the existing cycle detection functions in `Data.Graph.Algorithms`:

```purescript
module Data.Graph.Algorithms
  ( -- existing exports...
  , hasCycle
  , findCycle
  , isDAG
  -- new exports:
  , findBackEdges
  , findBackEdgesWeighted
  ) where
```

### 5. Test cases

Test with these patterns (all proven by the layout library's test suite):

```purescript
-- Simple cycle: A -> B -> C -> A
-- Back-edge: C -> A (or whichever DFS picks)
-- Result: Set contains one pair

-- Interior cycle: A -> B -> C -> D, B -> A
-- Back-edge: B -> A
-- Result: Set contains (B, A)

-- Multiple cycles sharing nodes: A -> B -> C -> D, C -> B, D -> A
-- Back-edges: C -> B and D -> A (two back-edges)
-- Result: Set contains both pairs

-- Acyclic graph: A -> B -> C -> D
-- Result: empty Set

-- Graph with shared sink (regression test for findCycle bug):
-- I -> R -> S, R -> Bp, S -> P, S -> R, P -> Ref, Ref -> O, Ref -> Bp
-- Back-edge: S -> R (the only cycle)
-- Result: Set contains (S, R)
-- NOTE: the library's findCycle returned a phantom cycle [Bp, O, Bp]
-- for this graph. This DFS implementation correctly identifies only S -> R.
```

### 6. Why this is better than iterative findCycle

The current approach in layout engines to find all back-edges was:
```
while findCycle(graph):
  remove one edge from the cycle
```

Problems:
1. `findCycle` has a bug (phantom cycles) — fixed separately but still a concern
2. O(k * (V+E)) where k = number of cycles — rebuilds graph each iteration
3. Non-deterministic: different removal orders produce different feedback arc sets

The DFS approach:
1. Single O(V+E) pass — finds ALL back-edges at once
2. Deterministic: DFS order determined by `Ord node`
3. No graph rebuild needed
4. The resulting back-edge set IS a feedback arc set (removing them makes the graph acyclic)

### 7. Relationship to findCycle

Both should coexist:
- `findCycle` answers: "give me one cycle" (useful for error messages, cycle detection)
- `findBackEdges` answers: "give me all back-edges" (useful for layout, feedback arc sets)
- `hasCycle` / `isDAG` remain as quick boolean checks
