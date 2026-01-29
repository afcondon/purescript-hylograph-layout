-- | HATS-based Layout Rendering
-- |
-- | Renders hierarchical and flow data using HATS trees instead of raw Halogen HTML.
-- | Each visualization fits within a 400x400 circular viewport.
-- | Supports coordinated highlighting across all views via HATS behaviors.
module Gallery.RenderHATS
  ( renderTreeHorizontal
  , renderTreeVertical
  , renderTreeRadial
  , renderClusterHorizontal
  , renderClusterVertical
  , renderClusterRadial
  , renderPack
  , renderSunburst
  , renderIcicle
  , renderTreemap
  , renderSankey
  , renderChord
  , renderEdgeBundle
  , renderAdjacency
  , clearLayout
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, pi, min)
import Data.Number.Format (toStringWith, fixed)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tree (Tree) as DataTree
import DataViz.Layout.Hierarchy.Cluster as Cluster
import DataViz.Layout.Hierarchy.Link as Link
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import DataViz.Layout.Hierarchy.Tree as TreeLayout
import DataViz.Layout.Hierarchy.Treemap as Treemap
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Sankey.Compute as Sankey
import DataViz.Layout.Sankey.Path as SankeyPath
import DataViz.Layout.Chord as Chord
import DataViz.Layout.Adjacency as Adjacency
import DataViz.Layout.Hierarchy.EdgeBundle as EdgeBundle
import Effect (Effect)
import Hylograph.HATS (Tree, elem, forEach, withBehaviors, onCoordinatedHighlight, HighlightClass(..)) as HATS
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick as HATSInterp
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Transform (clearContainer)

import Gallery.FlowData (SankeyData, MatrixData, EdgeBundleData)

-- =============================================================================
-- Constants
-- =============================================================================

-- Manuscript color palette
colorForestGreen :: String
colorForestGreen = "#2d5a27"

colorMalachite :: String
colorMalachite = "#0d6e6e"

colorVermillion :: String
colorVermillion = "#c23b22"

colorGoldLeaf :: String
colorGoldLeaf = "#c9a227"

colorTyrianPurple :: String
colorTyrianPurple = "#66023c"

colorInkSepia :: String
colorInkSepia = "#5c4033"

-- Muted manuscript palette for flow layouts at rest
mutedFlow :: Int -> String
mutedFlow i = case i `mod` 6 of
  0 -> "#a89f91"
  1 -> "#b5a99a"
  2 -> "#9a8f82"
  3 -> "#c2b5a5"
  4 -> "#8f857a"
  _ -> "#d4c9b8"

-- =============================================================================
-- Types
-- =============================================================================

-- | Node type for hierarchical layouts
type HierNode =
  { name :: String
  , path :: String
  , value :: Number
  , x :: Number
  , y :: Number
  , depth :: Int
  , height :: Int
  }

-- | Link type for connecting parent to child
type LinkData =
  { sourcePath :: String
  , sourceX :: Number
  , sourceY :: Number
  , targetPath :: String
  , targetX :: Number
  , targetY :: Number
  }

-- =============================================================================
-- Highlight Classification
-- =============================================================================

-- | Check if two paths are related (ancestor/descendant relationship)
isRelated :: String -> String -> Boolean
isRelated a b =
  contains (Pattern (a <> ".")) b ||
  contains (Pattern (b <> ".")) a ||
  contains (Pattern ("." <> a)) b ||
  contains (Pattern ("." <> b)) a

-- | Classify relationship for hierarchical layouts
classifyHierarchical :: String -> String -> HATS.HighlightClass
classifyHierarchical nodePath hoveredPath
  | hoveredPath == nodePath = HATS.Primary
  | isRelated hoveredPath nodePath = HATS.Related
  | otherwise = HATS.Dimmed

-- | Classify relationship for flow layouts (exact match only)
classifySimple :: String -> String -> HATS.HighlightClass
classifySimple nodeName hoveredName
  | hoveredName == nodeName = HATS.Primary
  | otherwise = HATS.Dimmed

-- =============================================================================
-- Tree Flattening
-- =============================================================================

flattenNodes :: forall r. DataTree.Tree { path :: String | r } -> Array { path :: String | r }
flattenNodes tree =
  let
    node = head tree
    children = Array.fromFoldable (tail tree)
    childNodes = children >>= flattenNodes
  in
    [node] <> childNodes

extractLinks
  :: forall r
   . DataTree.Tree { path :: String, x :: Number, y :: Number | r }
  -> Array LinkData
extractLinks tree =
  let
    node = head tree
    children = Array.fromFoldable (tail tree)
    childLinks = children <#> \child ->
      let childNode = head child
      in { sourcePath: node.path
         , sourceX: node.x
         , sourceY: node.y
         , targetPath: childNode.path
         , targetX: childNode.x
         , targetY: childNode.y
         }
    grandchildLinks = children >>= extractLinks
  in
    childLinks <> grandchildLinks

-- =============================================================================
-- Polar Coordinates
-- =============================================================================

polarToCartesian :: Number -> Number -> { x :: Number, y :: Number }
polarToCartesian angle radius =
  { x: radius * cos (angle - pi / 2.0)
  , y: radius * sin (angle - pi / 2.0)
  }

-- =============================================================================
-- Utility
-- =============================================================================

clearLayout :: String -> Effect Unit
clearLayout = clearContainer

fmt :: Number -> String
fmt = toStringWith (fixed 2)

-- =============================================================================
-- Tree Horizontal Layout
-- =============================================================================

renderTreeHorizontal :: String -> DataTree.Tree HierNode -> Effect Unit
renderTreeHorizontal selector inputTree = do
  let
    config = TreeLayout.defaultTreeConfig
      { size = { width: 350.0, height: 320.0 }, minSeparation = 1.0 }
    laidOut = TreeLayout.tree config inputTree
    nodes = flattenNodes laidOut
    links = extractLinks laidOut
    tree = buildTreeHorizontal 40.0 25.0 colorForestGreen nodes links
  _ <- HATSInterp.rerender selector tree
  pure unit

buildTreeHorizontal
  :: Number -> Number -> String -> Array HierNode -> Array LinkData -> HATS.Tree
buildTreeHorizontal offsetX offsetY color nodes links =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0
    , F.preserveAspectRatio "xMidYMid meet"
    ]
    [ HATS.elem Group
        [ F.transform ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
        [ linksLayer <> nodesLayer ]
    ]
  where
  -- Links with coordinated highlighting
  linksLayer = HATS.forEach "links" Path links _.targetPath \link ->
    let pathD = Link.linkBezierHorizontal link.sourceY link.sourceX link.targetY link.targetX
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: link.targetPath
             , classify: classifyHierarchical link.targetPath
             , group: Nothing
             }
         ] $
         HATS.elem Path
           [ F.d pathD, F.fill "none", F.stroke color
           , F.strokeWidth 1.0, F.opacity "0.6"
           , F.class_ "link"
           ] []

  nodesLayer = HATS.forEach "nodes" Group nodes _.path \node ->
    let nodeRadius = if node.height == 0 then 4.0 else 6.0
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: node.path
             , classify: classifyHierarchical node.path
             , group: Nothing
             }
         ] $
         HATS.elem Group
           [ F.class_ "node"
           , F.transform ("translate(" <> show node.y <> "," <> show node.x <> ")")
           ]
           [ HATS.elem Circle
               [ F.r nodeRadius, F.fill color
               , F.stroke "#fff", F.strokeWidth 1.5
               , F.style "cursor: pointer"
               ] []
           ]

-- =============================================================================
-- Tree Vertical Layout
-- =============================================================================

renderTreeVertical :: String -> DataTree.Tree HierNode -> Effect Unit
renderTreeVertical selector inputTree = do
  let
    config = TreeLayout.defaultTreeConfig
      { size = { width: 320.0, height: 350.0 }, minSeparation = 1.0 }
    laidOut = TreeLayout.tree config inputTree
    nodes = flattenNodes laidOut
    links = extractLinks laidOut
    tree = buildTreeVertical 40.0 25.0 colorForestGreen nodes links
  _ <- HATSInterp.rerender selector tree
  pure unit

buildTreeVertical
  :: Number -> Number -> String -> Array HierNode -> Array LinkData -> HATS.Tree
buildTreeVertical offsetX offsetY color nodes links =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group
        [ F.transform ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
        [ linksLayer <> nodesLayer ]
    ]
  where
  linksLayer = HATS.forEach "links" Path links _.targetPath \link ->
    let pathD = Link.linkBezierVertical link.sourceX link.sourceY link.targetX link.targetY
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: link.targetPath
             , classify: classifyHierarchical link.targetPath
             , group: Nothing
             }
         ] $
         HATS.elem Path
           [ F.d pathD, F.fill "none", F.stroke color
           , F.strokeWidth 1.0, F.opacity "0.6"
           , F.class_ "link"
           ] []

  nodesLayer = HATS.forEach "nodes" Group nodes _.path \node ->
    let nodeRadius = if node.height == 0 then 4.0 else 6.0
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: node.path
             , classify: classifyHierarchical node.path
             , group: Nothing
             }
         ] $
         HATS.elem Group
           [ F.class_ "node"
           , F.transform ("translate(" <> show node.x <> "," <> show node.y <> ")")
           ]
           [ HATS.elem Circle
               [ F.r nodeRadius, F.fill color
               , F.stroke "#fff", F.strokeWidth 1.5
               , F.style "cursor: pointer"
               ] []
           ]

-- =============================================================================
-- Tree Radial Layout
-- =============================================================================

renderTreeRadial :: String -> DataTree.Tree HierNode -> Effect Unit
renderTreeRadial selector inputTree = do
  let
    config = TreeLayout.defaultTreeConfig
      { size = { width: 2.0 * pi, height: 150.0 }, minSeparation = 1.0 }
    laidOut = TreeLayout.tree config inputTree
    nodes = flattenNodes laidOut
    links = extractLinks laidOut
    tree = buildTreeRadial colorGoldLeaf nodes links
  _ <- HATSInterp.rerender selector tree
  pure unit

buildTreeRadial :: String -> Array HierNode -> Array LinkData -> HATS.Tree
buildTreeRadial color nodes links =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group
        [ F.transform "translate(200,200)" ]
        [ linksLayer <> nodesLayer ]
    ]
  where
  linksLayer = HATS.forEach "links" Path links _.targetPath \link ->
    let
      sourcePos = polarToCartesian link.sourceX link.sourceY
      targetPos = polarToCartesian link.targetX link.targetY
      pathD = Link.linkBezierRadialCartesian sourcePos.x sourcePos.y targetPos.x targetPos.y
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: link.targetPath
             , classify: classifyHierarchical link.targetPath
             , group: Nothing
             }
         ] $
         HATS.elem Path
           [ F.d pathD, F.fill "none", F.stroke color
           , F.strokeWidth 1.0, F.opacity "0.6"
           , F.class_ "link"
           ] []

  nodesLayer = HATS.forEach "nodes" Circle nodes _.path \node ->
    let
      pos = polarToCartesian node.x node.y
      nodeRadius = if node.height == 0 then 4.0 else 6.0
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: node.path
             , classify: classifyHierarchical node.path
             , group: Nothing
             }
         ] $
         HATS.elem Circle
           [ F.cx pos.x, F.cy pos.y
           , F.r nodeRadius, F.fill color
           , F.class_ "node"
           , F.style "cursor: pointer"
           ] []

-- =============================================================================
-- Cluster Layouts (use same rendering as Tree, different layout algorithm)
-- =============================================================================

renderClusterHorizontal :: String -> DataTree.Tree HierNode -> Effect Unit
renderClusterHorizontal selector inputTree = do
  let
    config = Cluster.defaultClusterConfig
      { size = { width: 350.0, height: 320.0 }, minSeparation = 1.0 }
    laidOut = Cluster.cluster config inputTree
    nodes = flattenNodes laidOut
    links = extractLinks laidOut
    tree = buildTreeHorizontal 25.0 40.0 colorMalachite nodes links
  _ <- HATSInterp.rerender selector tree
  pure unit

renderClusterVertical :: String -> DataTree.Tree HierNode -> Effect Unit
renderClusterVertical selector inputTree = do
  let
    config = Cluster.defaultClusterConfig
      { size = { width: 320.0, height: 350.0 }, minSeparation = 1.0 }
    laidOut = Cluster.cluster config inputTree
    nodes = flattenNodes laidOut
    links = extractLinks laidOut
    tree = buildTreeVertical 40.0 25.0 colorMalachite nodes links
  _ <- HATSInterp.rerender selector tree
  pure unit

renderClusterRadial :: String -> DataTree.Tree HierNode -> Effect Unit
renderClusterRadial selector inputTree = do
  let
    config = Cluster.defaultClusterConfig
      { size = { width: 2.0 * pi, height: 150.0 }, minSeparation = 1.0 }
    laidOut = Cluster.cluster config inputTree
    nodes = flattenNodes laidOut
    links = extractLinks laidOut
    tree = buildTreeRadial colorMalachite nodes links
  _ <- HATSInterp.rerender selector tree
  pure unit

-- =============================================================================
-- Pack Layout
-- =============================================================================

-- | Pack node flattened for rendering
type PackNodeFlat =
  { path :: String
  , x :: Number
  , y :: Number
  , r :: Number
  , depth :: Int
  , isLeaf :: Boolean
  }

renderPack
  :: String -> Pack.HierarchyData { name :: String, path :: String } -> Effect Unit
renderPack selector hierData = do
  let
    hierarchy = Pack.hierarchy hierData
    config = Pack.defaultPackConfig { size = { width: 380.0, height: 380.0 }, padding = 2.0 }
    packed = Pack.pack config hierarchy
    nodes = flattenPackNodes packed
    tree = buildPack nodes
  _ <- HATSInterp.rerender selector tree
  pure unit

flattenPackNodes :: Pack.PackNode { name :: String, path :: String } -> Array PackNodeFlat
flattenPackNodes (Pack.PackNode node) =
  let
    thisNode = { path: node.data_.path, x: node.x, y: node.y, r: node.r
               , depth: node.depth, isLeaf: Array.null node.children }
    childNodes = node.children >>= flattenPackNodes
  in [thisNode] <> childNodes

buildPack :: Array PackNodeFlat -> HATS.Tree
buildPack nodes =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group [ F.transform "translate(10,10)" ]
        [ HATS.forEach "circles" Circle nodes _.path \node ->
            let
              fillColor = packColor node.depth
              fillOpacityVal = if node.isLeaf then "0.7" else "0.1"
            in HATS.withBehaviors
                 [ HATS.onCoordinatedHighlight
                     { identify: node.path
                     , classify: classifyHierarchical node.path
                     , group: Nothing
                     }
                 ] $
                 HATS.elem Circle
                   [ F.cx node.x, F.cy node.y, F.r node.r
                   , F.fill fillColor, F.fillOpacity fillOpacityVal
                   , F.stroke fillColor, F.strokeWidth 0.5
                   , F.class_ "node"
                   , F.style "cursor: pointer"
                   ] []
        ]
    ]
  where
  packColor d = case d `mod` 4 of
    0 -> colorVermillion
    1 -> "#d4766b"
    2 -> "#e8a89d"
    _ -> "#f5d4cf"

-- =============================================================================
-- Sunburst Layout
-- =============================================================================

type SunburstNodeFlat =
  { path :: String, x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number, depth :: Int }

renderSunburst
  :: String -> Partition.HierarchyData { name :: String, path :: String } -> Effect Unit
renderSunburst selector hierData = do
  let
    hierarchy = Partition.hierarchy hierData
    config = Partition.defaultPartitionConfig { size = { width: 1.0, height: 1.0 } }
    partitioned = Partition.partition config hierarchy
    nodes = flattenPartitionNodes partitioned
    tree = buildSunburst 170.0 nodes
  _ <- HATSInterp.rerender selector tree
  pure unit

flattenPartitionNodes :: Partition.PartitionNode { name :: String, path :: String } -> Array SunburstNodeFlat
flattenPartitionNodes (Partition.PartNode node) =
  let
    thisNode = { path: node.data_.path, x0: node.x0, y0: node.y0, x1: node.x1, y1: node.y1, depth: node.depth }
    childNodes = node.children >>= flattenPartitionNodes
  in [thisNode] <> childNodes

buildSunburst :: Number -> Array SunburstNodeFlat -> HATS.Tree
buildSunburst radius nodes =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group [ F.transform "translate(200,200)" ]
        [ HATS.forEach "arcs" Path nodes _.path \node ->
            let
              fillColor = sunburstColor node.depth
              arcPath = Partition.sunburstArcPath node.x0 node.y0 node.x1 node.y1 radius
            in HATS.withBehaviors
                 [ HATS.onCoordinatedHighlight
                     { identify: node.path
                     , classify: classifyHierarchical node.path
                     , group: Nothing
                     }
                 ] $
                 HATS.elem Path
                   [ F.d arcPath, F.fill fillColor
                   , F.stroke "#f5f0e1", F.strokeWidth 0.5
                   , F.class_ "node"
                   , F.style "cursor: pointer"
                   ] []
        ]
    ]
  where
  sunburstColor d = case d `mod` 4 of
    0 -> colorGoldLeaf
    1 -> "#d4b54a"
    2 -> "#e5ce7a"
    _ -> "#f2e6b3"

-- =============================================================================
-- Icicle Layout
-- =============================================================================

renderIcicle
  :: String -> Partition.HierarchyData { name :: String, path :: String } -> Effect Unit
renderIcicle selector hierData = do
  let
    hierarchy = Partition.hierarchy hierData
    config = Partition.defaultPartitionConfig { size = { width: 380.0, height: 380.0 } }
    partitioned = Partition.partition config hierarchy
    nodes = flattenPartitionNodes partitioned
    tree = buildIcicle nodes
  _ <- HATSInterp.rerender selector tree
  pure unit

buildIcicle :: Array SunburstNodeFlat -> HATS.Tree
buildIcicle nodes =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group [ F.transform "translate(10,10)" ]
        [ HATS.forEach "rects" Rect nodes _.path \node ->
            let
              fillColor = icicleColor node.depth
              w = node.x1 - node.x0
              h = node.y1 - node.y0
              nodeTree = if w > 0.5 && h > 0.5
                then HATS.elem Rect
                  [ F.x node.x0, F.y node.y0
                  , F.width w, F.height h
                  , F.fill fillColor, F.stroke "#f5f0e1", F.strokeWidth 0.5
                  , F.class_ "node"
                  , F.style "cursor: pointer"
                  ] []
                else HATS.elem Group [] []
            in HATS.withBehaviors
                 [ HATS.onCoordinatedHighlight
                     { identify: node.path
                     , classify: classifyHierarchical node.path
                     , group: Nothing
                     }
                 ] nodeTree
        ]
    ]
  where
  icicleColor d = case d `mod` 4 of
    0 -> colorGoldLeaf
    1 -> "#d4b54a"
    2 -> "#e5ce7a"
    _ -> "#f2e6b3"

-- =============================================================================
-- Treemap Layout
-- =============================================================================

type TreemapNodeFlat =
  { path :: String, x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number, depth :: Int, isLeaf :: Boolean }

renderTreemap
  :: String -> Partition.HierarchyData { name :: String, path :: String } -> Effect Unit
renderTreemap selector hierData = do
  let
    valuedNode = partitionToValuedNode hierData
    config = Treemap.defaultTreemapConfig
      { size = { width: 380.0, height: 380.0 }
      , tile = Treemap.squarify Treemap.phi
      , paddingInner = 1.0
      }
    treemapped = Treemap.treemap config valuedNode
    nodes = flattenTreemapNodes treemapped
    tree = buildTreemap nodes
  _ <- HATSInterp.rerender selector tree
  pure unit

partitionToValuedNode :: Partition.HierarchyData { name :: String, path :: String } -> ValuedNode { name :: String, path :: String }
partitionToValuedNode = go 0
  where
  go depth (Partition.HierarchyData node) =
    let
      kids = case node.children of
        Nothing -> []
        Just children -> map (go (depth + 1)) children
      childValues = foldl (\acc (VNode v) -> acc + v.value) 0.0 kids
      value = case node.value of
        Just v -> v
        Nothing -> if Array.null kids then 0.0 else childValues
      childHeights = map (\(VNode v) -> v.height) kids
      maxChildHeight = foldl max 0 childHeights
      height = if Array.null kids then 0 else maxChildHeight + 1
    in VNode { data_: node.data_, depth, height, parent: Nothing, children: kids, value }

flattenTreemapNodes :: Treemap.TreemapNode { name :: String, path :: String } -> Array TreemapNodeFlat
flattenTreemapNodes (Treemap.TNode node) =
  let
    thisNode = { path: node.data_.path, x0: node.x0, y0: node.y0, x1: node.x1, y1: node.y1
               , depth: node.depth, isLeaf: Array.null node.children }
    childNodes = node.children >>= flattenTreemapNodes
  in [thisNode] <> childNodes

buildTreemap :: Array TreemapNodeFlat -> HATS.Tree
buildTreemap nodes =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group [ F.transform "translate(10,10)" ]
        [ HATS.forEach "rects" Rect nodes _.path \node ->
            let
              fillColor = treemapColor node.depth
              w = node.x1 - node.x0
              h = node.y1 - node.y0
              nodeTree = if node.isLeaf && w > 1.0 && h > 1.0
                then HATS.elem Rect
                  [ F.x node.x0, F.y node.y0
                  , F.width w, F.height h
                  , F.fill fillColor, F.stroke "#f5f0e1", F.strokeWidth 0.5
                  , F.class_ "node"
                  , F.style "cursor: pointer"
                  ] []
                else HATS.elem Group [] []
            in HATS.withBehaviors
                 [ HATS.onCoordinatedHighlight
                     { identify: node.path
                     , classify: classifyHierarchical node.path
                     , group: Nothing
                     }
                 ] nodeTree
        ]
    ]
  where
  treemapColor d = case d `mod` 5 of
    0 -> colorTyrianPurple
    1 -> "#8c2d5e"
    2 -> "#a84a78"
    3 -> "#c46c96"
    _ -> "#e0a2c0"

-- =============================================================================
-- Sankey Diagram
-- =============================================================================

type SankeyNodeFlat = { name :: String, x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
type SankeyLinkFlat = { pathD :: String, sourceName :: String, targetName :: String }

renderSankey :: String -> SankeyData -> Effect Unit
renderSankey selector sankeyData = do
  let
    layoutResult = Sankey.computeLayout sankeyData.links 380.0 380.0
    nodeFlats = layoutResult.nodes <#> \n ->
      { name: n.name, x0: n.x0, y0: n.y0, x1: n.x1, y1: n.y1 }
    linkFlats = layoutResult.links <#> \link ->
      { pathD: SankeyPath.generateLinkPath layoutResult.nodes link
      , sourceName: fromMaybe "" $ map _.name $ SankeyPath.findNode layoutResult.nodes link.sourceIndex
      , targetName: fromMaybe "" $ map _.name $ SankeyPath.findNode layoutResult.nodes link.targetIndex
      }
    tree = buildSankey nodeFlats linkFlats
  _ <- HATSInterp.rerender selector tree
  pure unit

buildSankey :: Array SankeyNodeFlat -> Array SankeyLinkFlat -> HATS.Tree
buildSankey nodes links =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group [ F.transform "translate(10,10)" ]
        [ linksLayer <> nodesLayer ]
    ]
  where
  -- Links with coordinated highlighting - highlight when source or target is hovered
  linksLayer = HATS.forEach "sankey-links" Path links (\l -> l.sourceName <> "-" <> l.targetName) \link ->
    let
      linkIdentity = link.sourceName <> "-" <> link.targetName
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: linkIdentity
             , classify: \hoveredId ->
                 if hoveredId == linkIdentity then HATS.Primary
                 else if hoveredId == link.sourceName || hoveredId == link.targetName then HATS.Related
                 else HATS.Dimmed
             , group: Nothing
             }
         ] $
         HATS.elem Path
           [ F.d link.pathD
           , F.fill "rgba(168, 159, 145, 0.3)"
           , F.class_ "sankey-link"
           ] []

  nodesLayer = HATS.forEach "sankey-nodes" Rect nodes _.name \node ->
    let
      w = node.x1 - node.x0
      nodeH = node.y1 - node.y0
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: node.name
             , classify: classifySimple node.name
             , group: Nothing
             }
         ] $
         HATS.elem Rect
           [ F.x node.x0, F.y node.y0
           , F.width w, F.height nodeH
           , F.fill (mutedFlow 0)
           , F.class_ "sankey-node"
           , F.style "cursor: pointer"
           ] []

-- =============================================================================
-- Chord Diagram
-- =============================================================================

type ChordArcFlat = { name :: String, index :: Int, arcPath :: String }
type ChordRibbonFlat = { sourceName :: String, targetName :: String, ribbonPath :: String }

renderChord :: String -> MatrixData -> Effect Unit
renderChord selector matrixData = do
  let
    layoutResult = Chord.layout matrixData.matrix
    innerRadius = 150.0
    outerRadius = 170.0
    arcs = layoutResult.groups <#> \g ->
      { name: fromMaybe "" $ matrixData.names Array.!! g.index
      , index: g.index
      , arcPath: describeArc innerRadius outerRadius g.startAngle g.endAngle
      }
    ribbons = layoutResult.chords <#> \chord ->
      { sourceName: fromMaybe "" $ matrixData.names Array.!! chord.source.index
      , targetName: fromMaybe "" $ matrixData.names Array.!! chord.target.index
      , ribbonPath: describeRibbon innerRadius chord.source chord.target
      }
    tree = buildChord arcs ribbons
  _ <- HATSInterp.rerender selector tree
  pure unit

describeArc :: Number -> Number -> Number -> Number -> String
describeArc innerR outerR startAngle endAngle =
  let
    x1 = cos startAngle * outerR
    y1 = sin startAngle * outerR
    x2 = cos endAngle * outerR
    y2 = sin endAngle * outerR
    x3 = cos endAngle * innerR
    y3 = sin endAngle * innerR
    x4 = cos startAngle * innerR
    y4 = sin startAngle * innerR
    largeArc = if endAngle - startAngle > pi then "1" else "0"
  in
    "M" <> fmt x1 <> "," <> fmt y1
    <> " A" <> fmt outerR <> "," <> fmt outerR <> " 0 " <> largeArc <> " 1 " <> fmt x2 <> "," <> fmt y2
    <> " L" <> fmt x3 <> "," <> fmt y3
    <> " A" <> fmt innerR <> "," <> fmt innerR <> " 0 " <> largeArc <> " 0 " <> fmt x4 <> "," <> fmt y4
    <> " Z"

describeRibbon :: Number -> Chord.ChordGroup -> Chord.ChordGroup -> String
describeRibbon radius source target =
  let
    sx1 = cos source.startAngle * radius
    sy1 = sin source.startAngle * radius
    sx2 = cos source.endAngle * radius
    sy2 = sin source.endAngle * radius
    tx1 = cos target.startAngle * radius
    ty1 = sin target.startAngle * radius
    tx2 = cos target.endAngle * radius
    ty2 = sin target.endAngle * radius
  in
    "M" <> fmt sx1 <> "," <> fmt sy1
    <> " Q 0,0 " <> fmt tx1 <> "," <> fmt ty1
    <> " A" <> fmt radius <> "," <> fmt radius <> " 0 0 1 " <> fmt tx2 <> "," <> fmt ty2
    <> " Q 0,0 " <> fmt sx2 <> "," <> fmt sy2
    <> " A" <> fmt radius <> "," <> fmt radius <> " 0 0 1 " <> fmt sx1 <> "," <> fmt sy1
    <> " Z"

buildChord :: Array ChordArcFlat -> Array ChordRibbonFlat -> HATS.Tree
buildChord arcs ribbons =
  HATS.elem SVG
    [ F.attr "viewBox" "-200 -200 400 400", F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group []
        [ ribbonsLayer <> arcsLayer ]
    ]
  where
  -- Ribbons with coordinated highlighting
  ribbonsLayer = HATS.forEach "chord-ribbons" Path ribbons (\r -> r.sourceName <> "-" <> r.targetName) \ribbon ->
    let ribbonId = ribbon.sourceName <> "-" <> ribbon.targetName
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: ribbonId
             , classify: \hoveredId ->
                 if hoveredId == ribbonId then HATS.Primary
                 else if hoveredId == ribbon.sourceName || hoveredId == ribbon.targetName then HATS.Related
                 else HATS.Dimmed
             , group: Nothing
             }
         ] $
         HATS.elem Path
           [ F.d ribbon.ribbonPath
           , F.fill "rgba(168, 159, 145, 0.3)"
           , F.class_ "chord-ribbon"
           ] []

  arcsLayer = HATS.forEach "chord-arcs" Path arcs _.name \arc ->
    HATS.withBehaviors
      [ HATS.onCoordinatedHighlight
          { identify: arc.name
          , classify: classifySimple arc.name
          , group: Nothing
          }
      ] $
      HATS.elem Path
        [ F.d arc.arcPath
        , F.fill (mutedFlow arc.index)
        , F.class_ "chord-arc"
        , F.style "cursor: pointer"
        ] []

-- =============================================================================
-- Edge Bundle
-- =============================================================================

type BundleNodeFlat = { fullName :: String, cartX :: Number, cartY :: Number, isLeaf :: Boolean }
type BundleLinkFlat = { source :: String, target :: String, pathD :: String }

renderEdgeBundle :: String -> EdgeBundleData -> Effect Unit
renderEdgeBundle selector bundleData = do
  let
    config = { getName: _.name, getImports: _.imports, beta: 0.85, innerRadius: 50.0, outerRadius: 180.0 }
    result = EdgeBundle.edgeBundle config bundleData.nodes
    nodeFlats = Array.filter _.isLeaf result.nodes <#> \n ->
      { fullName: n.fullName, cartX: n.cartX, cartY: n.cartY, isLeaf: n.isLeaf }
    linkFlats = result.links <#> \l ->
      { source: l.source, target: l.target, pathD: l.path }
    tree = buildEdgeBundle nodeFlats linkFlats
  _ <- HATSInterp.rerender selector tree
  pure unit

buildEdgeBundle :: Array BundleNodeFlat -> Array BundleLinkFlat -> HATS.Tree
buildEdgeBundle nodes links =
  HATS.elem SVG
    [ F.attr "viewBox" "-200 -200 400 400", F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group []
        [ linksLayer <> nodesLayer ]
    ]
  where
  -- Links with coordinated highlighting
  linksLayer = HATS.forEach "bundle-links" Path links (\l -> l.source <> "-" <> l.target) \link ->
    let linkId = link.source <> "-" <> link.target
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: linkId
             , classify: \hoveredId ->
                 if hoveredId == linkId then HATS.Primary
                 else if hoveredId == link.source || hoveredId == link.target then HATS.Related
                 else HATS.Dimmed
             , group: Nothing
             }
         ] $
         HATS.elem Path
           [ F.d link.pathD, F.fill "none"
           , F.stroke "rgba(168, 159, 145, 0.15)"
           , F.strokeWidth 0.5
           , F.class_ "bundle-link"
           ] []

  nodesLayer = HATS.forEach "bundle-nodes" Circle nodes _.fullName \node ->
    HATS.withBehaviors
      [ HATS.onCoordinatedHighlight
          { identify: node.fullName
          , classify: classifySimple node.fullName
          , group: Nothing
          }
      ] $
      HATS.elem Circle
        [ F.cx node.cartX, F.cy node.cartY
        , F.r 3.0
        , F.fill (mutedFlow 0)
        , F.class_ "bundle-node"
        , F.style "cursor: pointer"
        ] []

-- =============================================================================
-- Adjacency Matrix
-- =============================================================================

type AdjCellFlat = { rowName :: String, colName :: String, value :: Number, x :: Number, y :: Number, w :: Number, h :: Number }
type AdjLabelFlat = { name :: String, displayName :: String, x :: Number, y :: Number, rotation :: Number, anchor :: String }

renderAdjacency :: String -> MatrixData -> Effect Unit
renderAdjacency selector matrixData = do
  let
    layoutConfig = Adjacency.defaultConfig { cellSize = 25.0, labelWidth = 80.0, labelHeight = 80.0 }
    layoutResult = Adjacency.layoutWithConfig layoutConfig { matrix: matrixData.matrix, names: matrixData.names }
    scale = 380.0 / layoutResult.totalWidth
    cells = layoutResult.cells <#> \c ->
      { rowName: c.rowName, colName: c.colName, value: c.value
      , x: c.position.x, y: c.position.y, w: c.position.width, h: c.position.height }
    rowLabels = layoutResult.rowLabels <#> \l ->
      { name: l.name, displayName: l.displayName
      , x: l.position.x, y: l.position.y, rotation: l.position.rotation, anchor: l.position.anchor }
    colLabels = layoutResult.colLabels <#> \l ->
      { name: l.name, displayName: l.displayName
      , x: l.position.x, y: l.position.y, rotation: l.position.rotation, anchor: l.position.anchor }
    tree = buildAdjacency scale cells rowLabels colLabels
  _ <- HATSInterp.rerender selector tree
  pure unit

buildAdjacency
  :: Number -> Array AdjCellFlat -> Array AdjLabelFlat -> Array AdjLabelFlat -> HATS.Tree
buildAdjacency scale cells rowLabels colLabels =
  HATS.elem SVG
    [ F.viewBox 0.0 0.0 400.0 400.0, F.preserveAspectRatio "xMidYMid meet" ]
    [ HATS.elem Group [ F.transform ("translate(10,10) scale(" <> fmt scale <> ")") ]
        [ cellsLayer <> rowLabelsLayer <> colLabelsLayer ]
    ]
  where
  cellsLayer = HATS.forEach "adj-cells" Rect cells (\c -> c.rowName <> "-" <> c.colName) \cell ->
    let
      intensity = min 1.0 (cell.value / 50.0)
      fillOpacityVal = if cell.value > 0.0 then fmt (0.2 + intensity * 0.8) else "0"
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: cell.rowName  -- Cells trigger on row name hover
             , classify: \hoveredId ->
                 if hoveredId == cell.rowName || hoveredId == cell.colName then HATS.Related
                 else HATS.Dimmed
             , group: Nothing
             }
         ] $
         HATS.elem Rect
           [ F.x cell.x, F.y cell.y
           , F.width cell.w, F.height cell.h
           , F.fill (mutedFlow 0), F.fillOpacity fillOpacityVal
           , F.stroke "#d4c9b8", F.strokeWidth 0.5
           , F.class_ "adjacency-cell"
           , F.style "cursor: pointer"
           ] []

  -- Row labels with coordinated highlighting
  rowLabelsLayer = HATS.forEach "row-labels" Text rowLabels _.name \label ->
    let
      txform = if label.rotation /= 0.0
        then "rotate(" <> fmt label.rotation <> " " <> fmt label.x <> " " <> fmt label.y <> ")"
        else ""
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: label.name
             , classify: classifySimple label.name
             , group: Nothing
             }
         ] $
         HATS.elem Text
           [ F.x label.x, F.y label.y
           , F.textAnchor label.anchor, F.fontSize "8"
           , F.fill colorInkSepia, F.fontWeight "normal"
           , F.transform txform, F.class_ "adjacency-label row-label"
           , F.style "cursor: pointer"
           ] []

  -- Column labels with coordinated highlighting
  colLabelsLayer = HATS.forEach "col-labels" Text colLabels _.name \label ->
    let
      txform = if label.rotation /= 0.0
        then "rotate(" <> fmt label.rotation <> " " <> fmt label.x <> " " <> fmt label.y <> ")"
        else ""
    in HATS.withBehaviors
         [ HATS.onCoordinatedHighlight
             { identify: label.name
             , classify: classifySimple label.name
             , group: Nothing
             }
         ] $
         HATS.elem Text
           [ F.x label.x, F.y label.y
           , F.textAnchor label.anchor, F.fontSize "8"
           , F.fill colorInkSepia, F.fontWeight "normal"
           , F.transform txform, F.class_ "adjacency-label col-label"
           , F.style "cursor: pointer"
           ] []
