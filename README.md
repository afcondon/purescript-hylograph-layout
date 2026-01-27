# purescript-psd3-layout

[![Layout Gallery Demo](../../site/lib-layout/public/demo.jpeg)](/layouts/)

Pure PureScript implementations of layout algorithms for hierarchies and flow diagrams. 

## Overview

Layout algorithms for hierarchical and graph data, implemented in pure PureScript. Many of these layouts are familiar to users of D3 but here they are implemented without FFI dependencies using FP implementations in 100% PureScript. These work with the rose-tree data structure from `purescript-tree-rose`.

## Installation

```bash
spago install psd3-layout
```

## Available Layouts

### Hierarchical Layouts

- **Tree** - Tidy tree layout (Reingold-Tilford algorithm)
- **Pack** - Circle packing for hierarchical data
- **Partition** - Sunburst/icicle partition layout
- **Treemap** - Multiple tiling algorithms: squarify (default), slice, dice, sliceDice, binary

### Graph Layouts

- **Sankey** - Flow diagram layout for directed acyclic graphs
- **EdgeBundle** - Hierarchical edge bundling

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
- `DataViz.Layout.Sankey` - Sankey diagrams
- `DataViz.Layout.EdgeBundle` - Edge bundling

## Part of PSD3

- **psd3-layout** - Layout algorithms (this package)
- **psd3-graph** - Graph algorithms and DAG support
- **psd3-selection** - D3 selection library
- **psd3-simulation** - Force simulation

Uses `tree-rose` for rose tree data structures.

## License

MIT
