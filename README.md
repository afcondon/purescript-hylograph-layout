# purescript-psd3-layout

Pure PureScript implementations of D3-style layout algorithms.

## Overview

Layout algorithms for hierarchical and graph data, implemented in pure PureScript without FFI dependencies on D3.js. These work with the psd3-tree data structures.

## Installation

```bash
spago install psd3-layout
```

## Available Layouts

### Hierarchical Layouts

- **Tree** - Tidy tree layout (Reingold-Tilford algorithm)
- **Pack** - Circle packing for hierarchical data
- **Partition** - Sunburst/icicle partition layout
- **Treemap** - Squarified treemap algorithm

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

- **psd3-tree** - Tree data structures (dependency)
- **psd3-layout** - Layout algorithms (this package)
- **psd3-selection** - D3 selection library
- **psd3-simulation** - Force simulation

## License

MIT
