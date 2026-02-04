# purescript-hylograph-layout

[![Layout Gallery Demo](../../site/lib-layout/public/demo.jpeg)](/layouts/)

Pure PureScript implementations of layout algorithms for hierarchies and flow diagrams.

## Overview

Layout algorithms for hierarchical and graph data, implemented in pure PureScript. Many of these layouts are familiar to users of D3 but here they are implemented without FFI dependencies using FP implementations in 100% PureScript. The hierarchical ones work with the rose-tree data structure from `purescript-tree-rose`.

## Installation

```bash
spago install hylograph-layout
```

## Available Layouts

### Hierarchical Layouts

- **Tree** - Tidy tree layout (Reingold-Tilford algorithm)
- **Pack** - Circle packing for hierarchical data
- **Partition** - Sunburst/icicle partition layout
- **Treemap** - Multiple tiling algorithms: squarify (default), slice, dice, sliceDice, binary
- **Cluster** - Dendrogram layout

### Graph Layouts

- **Sankey** - Flow diagram layout for directed acyclic graphs (uses `hylograph-graph` for DAG operations)
- **EdgeBundle** - Hierarchical edge bundling
- **Chord** - Chord diagram layout
- **Adjacency** - Adjacency matrix layout

## Example

```purescript
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import Data.Tree (mkTree)

myTree = mkTree "root" [mkTree "a" [], mkTree "b" []]
config = defaultTreeConfig { size = { width: 400.0, height: 300.0 } }
positioned = tree config myTree
-- Each node now has x, y coordinates
```

## Modules

- `DataViz.Layout.Hierarchy.Tree` - Tree layout
- `DataViz.Layout.Hierarchy.Pack` - Circle packing
- `DataViz.Layout.Hierarchy.Partition` - Partition/sunburst
- `DataViz.Layout.Hierarchy.Treemap` - Treemap
- `DataViz.Layout.Hierarchy.Cluster` - Dendrogram
- `DataViz.Layout.Hierarchy.EdgeBundle` - Edge bundling
- `DataViz.Layout.Sankey.Compute` - Sankey diagrams
- `DataViz.Layout.Chord` - Chord diagrams
- `DataViz.Layout.Adjacency` - Adjacency matrix

## Part of Hylograph

- **hylograph-layout** - Layout algorithms (this package)
- **hylograph-graph** - Graph algorithms and DAG support
- **hylograph-selection** - D3 selection library
- **hylograph-simulation** - Force simulation

Uses `tree-rose` for rose tree data structures.

## License

MIT
