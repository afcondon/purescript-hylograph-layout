-- | Types for the Layout Gallery
-- |
-- | 14 layout variants showing hierarchical data visualizations
module Gallery.Types where

import Prelude

-- | Layout types - includes vertical/horizontal/radial variants for tree-based layouts
data LayoutType
  = TreeHorizontal
  | TreeVertical
  | TreeRadial
  | ClusterHorizontal
  | ClusterVertical
  | ClusterRadial
  | Pack
  | PartitionSunburst
  | PartitionIcicle
  | Treemap
  | Chord
  | Sankey
  | EdgeBundle
  | Adjacency

derive instance eqLayoutType :: Eq LayoutType

-- | All layout types in display order
-- | Interspersed: tree variants mixed with other layouts
-- | Row 1 (4): TreeH, Pack, TreeV, Sunburst
-- | Row 2 (3 offset): TreeR, Icicle, ClusterH
-- | Row 3 (4): Treemap, ClusterV, Chord, ClusterR
-- | Row 4 (3 offset): Sankey, EdgeBundle, Adjacency
allLayouts :: Array LayoutType
allLayouts =
  [ TreeHorizontal, Pack, TreeVertical, PartitionSunburst
  , TreeRadial, PartitionIcicle, ClusterHorizontal
  , Treemap, ClusterVertical, Chord, ClusterRadial
  , Sankey, EdgeBundle, Adjacency
  ]

-- | Metadata for each layout type
type LayoutInfo =
  { name :: String
  , description :: String
  }

layoutInfo :: LayoutType -> LayoutInfo
layoutInfo = case _ of
  TreeHorizontal ->
    { name: "Tree"
    , description: "Reingold-Tilford"
    }
  TreeVertical ->
    { name: "Tree (Top-Down)"
    , description: "Vertical layout"
    }
  TreeRadial ->
    { name: "Radial Tree"
    , description: "Polar coordinates"
    }
  ClusterHorizontal ->
    { name: "Cluster"
    , description: "Dendrogram"
    }
  ClusterVertical ->
    { name: "Cluster (Top-Down)"
    , description: "Vertical dendrogram"
    }
  ClusterRadial ->
    { name: "Radial Cluster"
    , description: "Radial dendrogram"
    }
  Pack ->
    { name: "Circle Pack"
    , description: "Nested circles"
    }
  PartitionSunburst ->
    { name: "Sunburst"
    , description: "Radial partition"
    }
  PartitionIcicle ->
    { name: "Icicle"
    , description: "Linear partition"
    }
  Treemap ->
    { name: "Treemap"
    , description: "Squarified"
    }
  Chord ->
    { name: "Chord"
    , description: "Relationships"
    }
  Sankey ->
    { name: "Sankey"
    , description: "Flow diagram"
    }
  EdgeBundle ->
    { name: "Edge Bundle"
    , description: "Hierarchical bundling"
    }
  Adjacency ->
    { name: "Adjacency"
    , description: "Matrix view"
    }
