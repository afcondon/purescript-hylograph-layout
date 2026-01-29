-- | Gallery Layout Rendering
-- |
-- | Renders hierarchical data using hylograph-layout algorithms
-- | Each visualization fits within a 400x400 circular viewport
-- | Supports coordinated highlighting across all views
module Gallery.Render
  ( renderPlaceholder
  , renderTreeHorizontal
  , renderTreeVertical
  , renderTreeRadial
  , renderClusterHorizontal
  , renderClusterVertical
  , renderClusterRadial
  , renderPack
  , renderSunburst
  , renderIcicle
  , renderTreemap
  -- Flow layouts
  , renderSankey
  , renderChord
  , renderEdgeBundle
  , renderAdjacency
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, pi)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tree (Tree)
import DataViz.Layout.Hierarchy.Cluster as Cluster
import DataViz.Layout.Hierarchy.Link as Link
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import DataViz.Layout.Hierarchy.Tree as TreeLayout
import DataViz.Layout.Hierarchy.Treemap as Treemap
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Sankey.Compute as Sankey
import DataViz.Layout.Sankey.Path as SankeyPath
import DataViz.Layout.Sankey.Types (SankeyNode, SankeyLink)
import DataViz.Layout.Chord as Chord
import DataViz.Layout.Adjacency as Adjacency
import DataViz.Layout.Hierarchy.EdgeBundle as EdgeBundle
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Number.Format (toStringWith, fixed)

import Gallery.Types (LayoutType(..))
import Gallery.FlowData (SankeyData, MatrixData, EdgeBundleData, EdgeBundleNode)

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

colorOchre :: String
colorOchre = "#cc7722"

colorUltramarine :: String
colorUltramarine = "#1e3a5f"

colorInkSepia :: String
colorInkSepia = "#5c4033"

-- =============================================================================
-- SVG Helpers
-- =============================================================================

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

-- SVG element helpers with open row types for any props including events
svg :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svg = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "svg")

g :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
g = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "g")

circle :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
circle props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "circle") props []

rect :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
rect props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "rect") props []

path :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
path props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "path") props []

-- =============================================================================
-- Highlight Classification
-- =============================================================================

-- | Check if two paths are related (ancestor/descendant relationship)
isRelated :: String -> String -> Boolean
isRelated a b =
  contains (Pattern (a <> ".")) b ||  -- a is ancestor of b
  contains (Pattern (b <> ".")) a ||  -- b is ancestor of a
  contains (Pattern ("." <> a)) b ||  -- a is descendant of b
  contains (Pattern ("." <> b)) a     -- b is descendant of a

-- | Determine highlight class based on hovered path
-- | Primary = exact match, Related = ancestor/descendant, Dimmed = unrelated
highlightClass :: Maybe String -> String -> String
highlightClass Nothing _ = ""
highlightClass (Just hoveredPath) nodePath
  | hoveredPath == nodePath = "highlight-primary"
  | isRelated hoveredPath nodePath = "highlight-related"
  | otherwise = "highlight-dimmed"

-- =============================================================================
-- Placeholder
-- =============================================================================

renderPlaceholder :: forall w i. LayoutType -> HH.HTML w i
renderPlaceholder layoutType =
  svg
    [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
    , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
    ]
    [ circle
        [ HP.attr (HH.AttrName "cx") "200"
        , HP.attr (HH.AttrName "cy") "200"
        , HP.attr (HH.AttrName "r") "150"
        , HP.attr (HH.AttrName "fill") "none"
        , HP.attr (HH.AttrName "stroke") (placeholderColor layoutType)
        , HP.attr (HH.AttrName "stroke-width") "2"
        , HP.attr (HH.AttrName "stroke-dasharray") "8,4"
        , HP.attr (HH.AttrName "opacity") "0.5"
        ]
    ]
  where
  placeholderColor = case _ of
    TreeHorizontal -> colorForestGreen
    TreeVertical -> colorForestGreen
    TreeRadial -> colorGoldLeaf
    ClusterHorizontal -> colorMalachite
    ClusterVertical -> colorMalachite
    ClusterRadial -> colorMalachite
    Pack -> colorVermillion
    PartitionSunburst -> colorGoldLeaf
    PartitionIcicle -> colorGoldLeaf
    Treemap -> colorTyrianPurple
    Chord -> colorOchre
    Sankey -> colorUltramarine
    EdgeBundle -> colorMalachite
    Adjacency -> colorInkSepia

-- =============================================================================
-- Tree Layouts (Horizontal - left to right)
-- =============================================================================

renderTreeHorizontal
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  -> HH.HTML w i
renderTreeHorizontal hovered onHover onLeave inputTree =
  let
    config = TreeLayout.defaultTreeConfig
      { size = { width: 350.0, height: 320.0 }
      , minSeparation = 1.0
      }
    laidOut = TreeLayout.tree config inputTree
    offsetX = 40.0
    offsetY = 25.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          (renderHorizontalLinks laidOut colorForestGreen hovered
           <> renderHorizontalNodes laidOut colorForestGreen hovered onHover onLeave)
      ]

-- =============================================================================
-- Tree Layouts (Vertical - top to bottom)
-- =============================================================================

renderTreeVertical
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  -> HH.HTML w i
renderTreeVertical hovered onHover onLeave inputTree =
  let
    config = TreeLayout.defaultTreeConfig
      { size = { width: 320.0, height: 350.0 }
      , minSeparation = 1.0
      }
    laidOut = TreeLayout.tree config inputTree
    offsetX = 40.0
    offsetY = 25.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          (renderVerticalLinks laidOut colorForestGreen hovered
           <> renderVerticalNodes laidOut colorForestGreen hovered onHover onLeave)
      ]

-- =============================================================================
-- Tree Layouts (Radial)
-- =============================================================================

renderTreeRadial
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  -> HH.HTML w i
renderTreeRadial hovered onHover onLeave inputTree =
  let
    config = TreeLayout.defaultTreeConfig
      { size = { width: 2.0 * pi, height: 150.0 }
      , minSeparation = 1.0
      }
    laidOut = TreeLayout.tree config inputTree
    cx = 200.0
    cy = 200.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show cx <> "," <> show cy <> ")") ]
          (renderRadialLinks laidOut colorGoldLeaf hovered
           <> renderRadialNodes laidOut colorGoldLeaf hovered onHover onLeave)
      ]

-- =============================================================================
-- Link Rendering (Bezier curves)
-- =============================================================================

-- | Render bezier links for horizontal layouts
renderHorizontalLinks
  :: forall w i r
   . Tree { x :: Number, y :: Number, path :: String | r }
  -> String
  -> Maybe String
  -> Array (HH.HTML w i)
renderHorizontalLinks tree color hovered =
  let
    node = head tree
    children = tail tree
  in
    Array.fromFoldable children >>= \child ->
      let
        childNode = head child
        pathD = Link.linkBezierHorizontal node.y node.x childNode.y childNode.x
        linkClass = highlightClass hovered childNode.path
      in
        [ path
            [ HP.attr (HH.AttrName "d") pathD
            , HP.attr (HH.AttrName "fill") "none"
            , HP.attr (HH.AttrName "stroke") color
            , HP.attr (HH.AttrName "stroke-width") "1"
            , HP.attr (HH.AttrName "opacity") "0.6"
            , HP.attr (HH.AttrName "class") ("link " <> linkClass)
            ]
        ] <> renderHorizontalLinks child color hovered

-- | Render bezier links for vertical layouts
renderVerticalLinks
  :: forall w i r
   . Tree { x :: Number, y :: Number, path :: String | r }
  -> String
  -> Maybe String
  -> Array (HH.HTML w i)
renderVerticalLinks tree color hovered =
  let
    node = head tree
    children = tail tree
  in
    Array.fromFoldable children >>= \child ->
      let
        childNode = head child
        pathD = Link.linkBezierVertical node.x node.y childNode.x childNode.y
        linkClass = highlightClass hovered childNode.path
      in
        [ path
            [ HP.attr (HH.AttrName "d") pathD
            , HP.attr (HH.AttrName "fill") "none"
            , HP.attr (HH.AttrName "stroke") color
            , HP.attr (HH.AttrName "stroke-width") "1"
            , HP.attr (HH.AttrName "opacity") "0.6"
            , HP.attr (HH.AttrName "class") ("link " <> linkClass)
            ]
        ] <> renderVerticalLinks child color hovered

-- | Convert polar to cartesian for radial layouts
polarToCartesian :: Number -> Number -> { x :: Number, y :: Number }
polarToCartesian angle radius =
  { x: radius * cos (angle - pi / 2.0)
  , y: radius * sin (angle - pi / 2.0)
  }

-- | Render bezier links for radial layouts
renderRadialLinks
  :: forall w i r
   . Tree { x :: Number, y :: Number, path :: String | r }
  -> String
  -> Maybe String
  -> Array (HH.HTML w i)
renderRadialLinks tree color hovered =
  let
    node = head tree
    children = tail tree
    pos = polarToCartesian node.x node.y
  in
    Array.fromFoldable children >>= \child ->
      let
        childNode = head child
        childPos = polarToCartesian childNode.x childNode.y
        pathD = Link.linkBezierRadialCartesian pos.x pos.y childPos.x childPos.y
        linkClass = highlightClass hovered childNode.path
      in
        [ path
            [ HP.attr (HH.AttrName "d") pathD
            , HP.attr (HH.AttrName "fill") "none"
            , HP.attr (HH.AttrName "stroke") color
            , HP.attr (HH.AttrName "stroke-width") "1"
            , HP.attr (HH.AttrName "opacity") "0.6"
            , HP.attr (HH.AttrName "class") ("link " <> linkClass)
            ]
        ] <> renderRadialLinks child color hovered

-- =============================================================================
-- Node Rendering with Mouse Events
-- =============================================================================

-- | Render nodes for horizontal layouts with hover events
renderHorizontalNodes
  :: forall w i r
   . Tree { x :: Number, y :: Number, depth :: Int, path :: String | r }
  -> String
  -> Maybe String
  -> (String -> i)
  -> i
  -> Array (HH.HTML w i)
renderHorizontalNodes tree color hovered onHover onLeave =
  let
    node = head tree
    children = tail tree
    radius = max 2.0 (5.0 - toNumber node.depth * 0.4)
    nodeClass = highlightClass hovered node.path
  in
    [ circle
        [ HP.attr (HH.AttrName "cx") (show node.y)
        , HP.attr (HH.AttrName "cy") (show node.x)
        , HP.attr (HH.AttrName "r") (show radius)
        , HP.attr (HH.AttrName "fill") color
        , HP.attr (HH.AttrName "class") ("node " <> nodeClass)
        , HE.onMouseEnter \_ -> onHover node.path
        , HE.onMouseLeave \_ -> onLeave
        ]
    ] <> (Array.fromFoldable children >>= \child -> renderHorizontalNodes child color hovered onHover onLeave)

-- | Render nodes for vertical layouts with hover events
renderVerticalNodes
  :: forall w i r
   . Tree { x :: Number, y :: Number, depth :: Int, path :: String | r }
  -> String
  -> Maybe String
  -> (String -> i)
  -> i
  -> Array (HH.HTML w i)
renderVerticalNodes tree color hovered onHover onLeave =
  let
    node = head tree
    children = tail tree
    radius = max 2.0 (5.0 - toNumber node.depth * 0.4)
    nodeClass = highlightClass hovered node.path
  in
    [ circle
        [ HP.attr (HH.AttrName "cx") (show node.x)
        , HP.attr (HH.AttrName "cy") (show node.y)
        , HP.attr (HH.AttrName "r") (show radius)
        , HP.attr (HH.AttrName "fill") color
        , HP.attr (HH.AttrName "class") ("node " <> nodeClass)
        , HE.onMouseEnter \_ -> onHover node.path
        , HE.onMouseLeave \_ -> onLeave
        ]
    ] <> (Array.fromFoldable children >>= \child -> renderVerticalNodes child color hovered onHover onLeave)

-- | Render nodes for radial layouts with hover events
renderRadialNodes
  :: forall w i r
   . Tree { x :: Number, y :: Number, depth :: Int, path :: String | r }
  -> String
  -> Maybe String
  -> (String -> i)
  -> i
  -> Array (HH.HTML w i)
renderRadialNodes tree color hovered onHover onLeave =
  let
    node = head tree
    children = tail tree
    pos = polarToCartesian node.x node.y
    radius = max 2.0 (5.0 - toNumber node.depth * 0.4)
    nodeClass = highlightClass hovered node.path
  in
    [ circle
        [ HP.attr (HH.AttrName "cx") (show pos.x)
        , HP.attr (HH.AttrName "cy") (show pos.y)
        , HP.attr (HH.AttrName "r") (show radius)
        , HP.attr (HH.AttrName "fill") color
        , HP.attr (HH.AttrName "class") ("node " <> nodeClass)
        , HE.onMouseEnter \_ -> onHover node.path
        , HE.onMouseLeave \_ -> onLeave
        ]
    ] <> (Array.fromFoldable children >>= \child -> renderRadialNodes child color hovered onHover onLeave)

-- =============================================================================
-- Cluster Layouts
-- =============================================================================

renderClusterHorizontal
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  -> HH.HTML w i
renderClusterHorizontal hovered onHover onLeave inputTree =
  let
    config = Cluster.defaultClusterConfig
      { size = { width: 350.0, height: 320.0 }
      , minSeparation = 1.0
      }
    laidOut = Cluster.cluster config inputTree
    offsetX = 25.0
    offsetY = 40.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          (renderHorizontalLinks laidOut colorMalachite hovered
           <> renderHorizontalNodes laidOut colorMalachite hovered onHover onLeave)
      ]

renderClusterVertical
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  -> HH.HTML w i
renderClusterVertical hovered onHover onLeave inputTree =
  let
    config = Cluster.defaultClusterConfig
      { size = { width: 320.0, height: 350.0 }
      , minSeparation = 1.0
      }
    laidOut = Cluster.cluster config inputTree
    offsetX = 40.0
    offsetY = 25.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          (renderVerticalLinks laidOut colorMalachite hovered
           <> renderVerticalNodes laidOut colorMalachite hovered onHover onLeave)
      ]

renderClusterRadial
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  -> HH.HTML w i
renderClusterRadial hovered onHover onLeave inputTree =
  let
    config = Cluster.defaultClusterConfig
      { size = { width: 2.0 * pi, height: 150.0 }
      , minSeparation = 1.0
      }
    laidOut = Cluster.cluster config inputTree
    cx = 200.0
    cy = 200.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show cx <> "," <> show cy <> ")") ]
          (renderRadialLinks laidOut colorMalachite hovered
           <> renderRadialNodes laidOut colorMalachite hovered onHover onLeave)
      ]

-- =============================================================================
-- Pack Layout
-- =============================================================================

renderPack
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Pack.HierarchyData { name :: String, path :: String }
  -> HH.HTML w i
renderPack hovered onHover onLeave hierData =
  let
    hierarchy = Pack.hierarchy hierData
    config = Pack.defaultPackConfig
      { size = { width: 380.0, height: 380.0 }
      , padding = 2.0
      }
    packed = Pack.pack config hierarchy
    offsetX = 10.0
    offsetY = 10.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          (renderPackCircles packed hovered onHover onLeave)
      ]

-- | Render circles for pack layout with hover events
renderPackCircles
  :: forall w i
   . Pack.PackNode { name :: String, path :: String }
  -> Maybe String
  -> (String -> i)
  -> i
  -> Array (HH.HTML w i)
renderPackCircles (Pack.PackNode node) hovered onHover onLeave =
  let
    fillColor = depthColor node.depth
    fillOpacity = if Array.null node.children then "0.7" else "0.1"
    strokeColor = depthColor node.depth
    nodeClass = highlightClass hovered node.data_.path
  in
    [ circle
        [ HP.attr (HH.AttrName "cx") (show node.x)
        , HP.attr (HH.AttrName "cy") (show node.y)
        , HP.attr (HH.AttrName "r") (show node.r)
        , HP.attr (HH.AttrName "fill") fillColor
        , HP.attr (HH.AttrName "fill-opacity") fillOpacity
        , HP.attr (HH.AttrName "stroke") strokeColor
        , HP.attr (HH.AttrName "stroke-width") "0.5"
        , HP.attr (HH.AttrName "class") ("node " <> nodeClass)
        , HE.onMouseEnter \_ -> onHover node.data_.path
        , HE.onMouseLeave \_ -> onLeave
        ]
    ] <> (node.children >>= \child -> renderPackCircles child hovered onHover onLeave)
  where
  depthColor d = case d `mod` 4 of
    0 -> colorVermillion
    1 -> "#d4766b"
    2 -> "#e8a89d"
    _ -> "#f5d4cf"

-- =============================================================================
-- Partition Sunburst
-- =============================================================================

renderSunburst
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Partition.HierarchyData { name :: String, path :: String }
  -> HH.HTML w i
renderSunburst hovered onHover onLeave hierData =
  let
    hierarchy = Partition.hierarchy hierData
    config = Partition.defaultPartitionConfig
      { size = { width: 1.0, height: 1.0 }
      }
    partitioned = Partition.partition config hierarchy
    cx = 200.0
    cy = 200.0
    radius = 170.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show cx <> "," <> show cy <> ")") ]
          (renderSunburstArcs partitioned radius hovered onHover onLeave)
      ]

-- | Render arcs for sunburst with hover events
renderSunburstArcs
  :: forall w i
   . Partition.PartitionNode { name :: String, path :: String }
  -> Number
  -> Maybe String
  -> (String -> i)
  -> i
  -> Array (HH.HTML w i)
renderSunburstArcs (Partition.PartNode node) radius hovered onHover onLeave =
  let
    fillColor = sunburstColor node.depth
    arcPath = Partition.sunburstArcPath node.x0 node.y0 node.x1 node.y1 radius
    nodeClass = highlightClass hovered node.data_.path
  in
    [ path
        [ HP.attr (HH.AttrName "d") arcPath
        , HP.attr (HH.AttrName "fill") fillColor
        , HP.attr (HH.AttrName "stroke") "#f5f0e1"
        , HP.attr (HH.AttrName "stroke-width") "0.5"
        , HP.attr (HH.AttrName "class") ("node " <> nodeClass)
        , HE.onMouseEnter \_ -> onHover node.data_.path
        , HE.onMouseLeave \_ -> onLeave
        ]
    ] <> (node.children >>= \child -> renderSunburstArcs child radius hovered onHover onLeave)
  where
  sunburstColor d = case d `mod` 4 of
    0 -> colorGoldLeaf
    1 -> "#d4b54a"
    2 -> "#e5ce7a"
    _ -> "#f2e6b3"

-- =============================================================================
-- Partition Icicle
-- =============================================================================

renderIcicle
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Partition.HierarchyData { name :: String, path :: String }
  -> HH.HTML w i
renderIcicle hovered onHover onLeave hierData =
  let
    hierarchy = Partition.hierarchy hierData
    config = Partition.defaultPartitionConfig
      { size = { width: 380.0, height: 380.0 }
      }
    partitioned = Partition.partition config hierarchy
    offsetX = 10.0
    offsetY = 10.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          (renderIcicleRects partitioned hovered onHover onLeave)
      ]

-- | Render rectangles for icicle chart with hover events
renderIcicleRects
  :: forall w i
   . Partition.PartitionNode { name :: String, path :: String }
  -> Maybe String
  -> (String -> i)
  -> i
  -> Array (HH.HTML w i)
renderIcicleRects (Partition.PartNode node) hovered onHover onLeave =
  let
    fillColor = icicleColor node.depth
    width = node.x1 - node.x0
    height = node.y1 - node.y0
    nodeClass = highlightClass hovered node.data_.path
  in
    (if width > 0.5 && height > 0.5
      then
        [ rect
            [ HP.attr (HH.AttrName "x") (show node.x0)
            , HP.attr (HH.AttrName "y") (show node.y0)
            , HP.attr (HH.AttrName "width") (show width)
            , HP.attr (HH.AttrName "height") (show height)
            , HP.attr (HH.AttrName "fill") fillColor
            , HP.attr (HH.AttrName "stroke") "#f5f0e1"
            , HP.attr (HH.AttrName "stroke-width") "0.5"
            , HP.attr (HH.AttrName "class") ("node " <> nodeClass)
            , HE.onMouseEnter \_ -> onHover node.data_.path
            , HE.onMouseLeave \_ -> onLeave
            ]
        ]
      else [])
    <> (node.children >>= \child -> renderIcicleRects child hovered onHover onLeave)
  where
  icicleColor d = case d `mod` 4 of
    0 -> colorGoldLeaf
    1 -> "#d4b54a"
    2 -> "#e5ce7a"
    _ -> "#f2e6b3"

-- =============================================================================
-- Treemap Layout
-- =============================================================================

renderTreemap
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Partition.HierarchyData { name :: String, path :: String }
  -> HH.HTML w i
renderTreemap hovered onHover onLeave hierData =
  let
    valuedNode = partitionToValuedNode hierData
    config = Treemap.defaultTreemapConfig
      { size = { width: 380.0, height: 380.0 }
      , tile = Treemap.squarify Treemap.phi
      , paddingInner = 1.0
      }
    treemapped = Treemap.treemap config valuedNode
    offsetX = 10.0
    offsetY = 10.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(" <> show offsetX <> "," <> show offsetY <> ")") ]
          (renderTreemapRects treemapped hovered onHover onLeave)
      ]

-- | Convert Partition.HierarchyData to ValuedNode
partitionToValuedNode :: Partition.HierarchyData { name :: String, path :: String } -> ValuedNode { name :: String, path :: String }
partitionToValuedNode = go 0
  where
  go :: Int -> Partition.HierarchyData { name :: String, path :: String } -> ValuedNode { name :: String, path :: String }
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
    in
      VNode
        { data_: node.data_
        , depth: depth
        , height: height
        , parent: Nothing
        , children: kids
        , value: value
        }

-- | Render rectangles for treemap with hover events
renderTreemapRects
  :: forall w i
   . Treemap.TreemapNode { name :: String, path :: String }
  -> Maybe String
  -> (String -> i)
  -> i
  -> Array (HH.HTML w i)
renderTreemapRects (Treemap.TNode node) hovered onHover onLeave =
  let
    fillColor = treemapColor node.depth
    width = node.x1 - node.x0
    height = node.y1 - node.y0
    isLeaf = Array.null node.children
    nodeClass = highlightClass hovered node.data_.path
  in
    (if isLeaf && width > 1.0 && height > 1.0
      then
        [ rect
            [ HP.attr (HH.AttrName "x") (show node.x0)
            , HP.attr (HH.AttrName "y") (show node.y0)
            , HP.attr (HH.AttrName "width") (show width)
            , HP.attr (HH.AttrName "height") (show height)
            , HP.attr (HH.AttrName "fill") fillColor
            , HP.attr (HH.AttrName "stroke") "#f5f0e1"
            , HP.attr (HH.AttrName "stroke-width") "0.5"
            , HP.attr (HH.AttrName "class") ("node " <> nodeClass)
            , HE.onMouseEnter \_ -> onHover node.data_.path
            , HE.onMouseLeave \_ -> onLeave
            ]
        ]
      else [])
    <> (node.children >>= \child -> renderTreemapRects child hovered onHover onLeave)
  where
  treemapColor d = case d `mod` 5 of
    0 -> colorTyrianPurple
    1 -> "#8c2d5e"
    2 -> "#a84a78"
    3 -> "#c46c96"
    _ -> "#e0a2c0"

-- =============================================================================
-- Flow Layouts
-- =============================================================================

-- ColorBrewer Set2 palette for hover effects
colorBrewerSet2 :: Int -> String
colorBrewerSet2 i = case i `mod` 8 of
  0 -> "#66c2a5"  -- Teal
  1 -> "#fc8d62"  -- Orange
  2 -> "#8da0cb"  -- Blue
  3 -> "#e78ac3"  -- Pink
  4 -> "#a6d854"  -- Lime
  5 -> "#ffd92f"  -- Yellow
  6 -> "#e5c494"  -- Tan
  _ -> "#b3b3b3"  -- Gray

-- Muted manuscript palette for flow layouts at rest
mutedFlow :: Int -> String
mutedFlow i = case i `mod` 6 of
  0 -> "#a89f91"  -- Warm gray
  1 -> "#b5a99a"  -- Light sepia
  2 -> "#9a8f82"  -- Medium sepia
  3 -> "#c2b5a5"  -- Pale tan
  4 -> "#8f857a"  -- Dark sepia
  _ -> "#d4c9b8"  -- Light parchment

-- =============================================================================
-- Sankey Diagram
-- =============================================================================

renderSankey
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> SankeyData
  -> HH.HTML w i
renderSankey hovered onHover onLeave sankeyData =
  let
    -- Compute layout (fitting to 380x380 to leave margin)
    layoutResult = Sankey.computeLayout sankeyData.links 380.0 380.0
    nodes = layoutResult.nodes
    links = layoutResult.links
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") "translate(10, 10)" ]
          ( (links <#> renderSankeyLink nodes hovered onHover onLeave)
            <> (nodes <#> renderSankeyNode hovered onHover onLeave)
          )
      ]

renderSankeyNode
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> SankeyNode
  -> HH.HTML w i
renderSankeyNode hovered onHover onLeave node =
  let
    width = node.x1 - node.x0
    height = node.y1 - node.y0
    nodeClass = sankeyHighlight hovered node.name
    fillColor = case hovered of
      Just h | h == node.name -> colorBrewerSet2 0  -- Bright on hover
      Just _ -> mutedFlow 0  -- Dimmed when other hovered
      Nothing -> mutedFlow 0  -- Muted at rest
  in
    rect
      [ HP.attr (HH.AttrName "x") (toStringWith (fixed 2) node.x0)
      , HP.attr (HH.AttrName "y") (toStringWith (fixed 2) node.y0)
      , HP.attr (HH.AttrName "width") (toStringWith (fixed 2) width)
      , HP.attr (HH.AttrName "height") (toStringWith (fixed 2) height)
      , HP.attr (HH.AttrName "fill") fillColor
      , HP.attr (HH.AttrName "class") ("sankey-node " <> nodeClass)
      , HE.onMouseEnter \_ -> onHover node.name
      , HE.onMouseLeave \_ -> onLeave
      ]

renderSankeyLink
  :: forall w i
   . Array SankeyNode
  -> Maybe String
  -> (String -> i)
  -> i
  -> SankeyLink
  -> HH.HTML w i
renderSankeyLink nodes hovered onHover onLeave link =
  let
    pathD = SankeyPath.generateLinkPath nodes link
    sourceName = case SankeyPath.findNode nodes link.sourceIndex of
      Just n -> n.name
      Nothing -> ""
    targetName = case SankeyPath.findNode nodes link.targetIndex of
      Just n -> n.name
      Nothing -> ""
    isHighlighted = case hovered of
      Just h -> h == sourceName || h == targetName
      Nothing -> false
    fillColor = if isHighlighted then "rgba(102, 194, 165, 0.6)" else "rgba(168, 159, 145, 0.3)"
  in
    path
      [ HP.attr (HH.AttrName "d") pathD
      , HP.attr (HH.AttrName "fill") fillColor
      , HP.attr (HH.AttrName "class") "sankey-link"
      ]

sankeyHighlight :: Maybe String -> String -> String
sankeyHighlight Nothing _ = ""
sankeyHighlight (Just h) name
  | h == name = "highlight-primary"
  | otherwise = "highlight-dimmed"

-- =============================================================================
-- Chord Diagram
-- =============================================================================

renderChord
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> MatrixData
  -> HH.HTML w i
renderChord hovered onHover onLeave matrixData =
  let
    layoutResult = Chord.layout matrixData.matrix
    groups = layoutResult.groups
    chords = layoutResult.chords
    innerRadius = 150.0
    outerRadius = 170.0
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "-200 -200 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g []
          ( (groups <#> renderChordArc matrixData.names innerRadius outerRadius hovered onHover onLeave)
            <> (chords <#> renderChordRibbon matrixData.names innerRadius hovered onHover onLeave)
          )
      ]

renderChordArc
  :: forall w i
   . Array String
  -> Number
  -> Number
  -> Maybe String
  -> (String -> i)
  -> i
  -> Chord.ChordGroup
  -> HH.HTML w i
renderChordArc names innerR outerR hovered onHover onLeave group =
  let
    name = fromMaybe "" $ names Array.!! group.index
    arcPath = describeArc innerR outerR group.startAngle group.endAngle
    isHighlighted = hovered == Just name
    fillColor = if isHighlighted
      then colorBrewerSet2 group.index
      else mutedFlow group.index
  in
    path
      [ HP.attr (HH.AttrName "d") arcPath
      , HP.attr (HH.AttrName "fill") fillColor
      , HP.attr (HH.AttrName "class") "chord-arc"
      , HE.onMouseEnter \_ -> onHover name
      , HE.onMouseLeave \_ -> onLeave
      ]

renderChordRibbon
  :: forall w i
   . Array String
  -> Number
  -> Maybe String
  -> (String -> i)
  -> i
  -> Chord.Chord
  -> HH.HTML w i
renderChordRibbon names radius hovered onHover onLeave chord =
  let
    sourceName = fromMaybe "" $ names Array.!! chord.source.index
    targetName = fromMaybe "" $ names Array.!! chord.target.index
    ribbonPath = describeRibbon radius chord.source chord.target
    isHighlighted = case hovered of
      Just h -> h == sourceName || h == targetName
      Nothing -> false
    fillColor = if isHighlighted
      then "rgba(102, 194, 165, 0.7)"
      else "rgba(168, 159, 145, 0.3)"
  in
    path
      [ HP.attr (HH.AttrName "d") ribbonPath
      , HP.attr (HH.AttrName "fill") fillColor
      , HP.attr (HH.AttrName "class") "chord-ribbon"
      ]

-- SVG arc path description
describeArc :: Number -> Number -> Number -> Number -> String
describeArc innerR outerR startAngle endAngle =
  let
    -- Convert to Cartesian coordinates
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

-- SVG ribbon path for chord diagram
describeRibbon :: Number -> Chord.ChordGroup -> Chord.ChordGroup -> String
describeRibbon radius source target =
  let
    sa1 = source.startAngle
    sa2 = source.endAngle
    ta1 = target.startAngle
    ta2 = target.endAngle
    -- Source arc points
    sx1 = cos sa1 * radius
    sy1 = sin sa1 * radius
    sx2 = cos sa2 * radius
    sy2 = sin sa2 * radius
    -- Target arc points
    tx1 = cos ta1 * radius
    ty1 = sin ta1 * radius
    tx2 = cos ta2 * radius
    ty2 = sin ta2 * radius
  in
    "M" <> fmt sx1 <> "," <> fmt sy1
    <> " Q 0,0 " <> fmt tx1 <> "," <> fmt ty1
    <> " A" <> fmt radius <> "," <> fmt radius <> " 0 0 1 " <> fmt tx2 <> "," <> fmt ty2
    <> " Q 0,0 " <> fmt sx2 <> "," <> fmt sy2
    <> " A" <> fmt radius <> "," <> fmt radius <> " 0 0 1 " <> fmt sx1 <> "," <> fmt sy1
    <> " Z"

fmt :: Number -> String
fmt = toStringWith (fixed 2)

-- =============================================================================
-- Edge Bundle
-- =============================================================================

renderEdgeBundle
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> EdgeBundleData
  -> HH.HTML w i
renderEdgeBundle hovered onHover onLeave bundleData =
  let
    config =
      { getName: _.name
      , getImports: _.imports
      , beta: 0.85
      , innerRadius: 50.0
      , outerRadius: 180.0
      }
    result = EdgeBundle.edgeBundle config bundleData.nodes
    leafNodes = Array.filter _.isLeaf result.nodes
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "-200 -200 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g []
          ( (result.links <#> renderBundleLink hovered)
            <> (leafNodes <#> renderBundleNode hovered onHover onLeave)
          )
      ]

renderBundleNode
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> EdgeBundle.PositionedNode EdgeBundleNode
  -> HH.HTML w i
renderBundleNode hovered onHover onLeave node =
  let
    isHighlighted = hovered == Just node.fullName
    fillColor = if isHighlighted then colorBrewerSet2 0 else mutedFlow 0
    r = if isHighlighted then "5" else "3"
  in
    circle
      [ HP.attr (HH.AttrName "cx") (fmt node.cartX)
      , HP.attr (HH.AttrName "cy") (fmt node.cartY)
      , HP.attr (HH.AttrName "r") r
      , HP.attr (HH.AttrName "fill") fillColor
      , HP.attr (HH.AttrName "class") "bundle-node"
      , HE.onMouseEnter \_ -> onHover node.fullName
      , HE.onMouseLeave \_ -> onLeave
      ]

renderBundleLink
  :: forall w i
   . Maybe String
  -> EdgeBundle.BundledLink
  -> HH.HTML w i
renderBundleLink hovered link =
  let
    isHighlighted = case hovered of
      Just h -> h == link.source || h == link.target
      Nothing -> false
    strokeColor = if isHighlighted then "rgba(102, 194, 165, 0.8)" else "rgba(168, 159, 145, 0.15)"
    strokeWidth = if isHighlighted then "1.5" else "0.5"
  in
    path
      [ HP.attr (HH.AttrName "d") link.path
      , HP.attr (HH.AttrName "fill") "none"
      , HP.attr (HH.AttrName "stroke") strokeColor
      , HP.attr (HH.AttrName "stroke-width") strokeWidth
      , HP.attr (HH.AttrName "class") "bundle-link"
      ]

-- =============================================================================
-- Adjacency Matrix
-- =============================================================================

renderAdjacency
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> MatrixData
  -> HH.HTML w i
renderAdjacency hovered onHover onLeave matrixData =
  let
    layoutConfig = Adjacency.defaultConfig { cellSize = 25.0, labelWidth = 80.0, labelHeight = 80.0 }
    layoutResult = Adjacency.layoutWithConfig layoutConfig
      { matrix: matrixData.matrix, names: matrixData.names }
    -- Scale to fit viewport
    scale = 380.0 / layoutResult.totalWidth
  in
    svg
      [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
      , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
      ]
      [ g [ HP.attr (HH.AttrName "transform") ("translate(10,10) scale(" <> fmt scale <> ")") ]
          ( (layoutResult.cells <#> renderAdjacencyCell hovered onHover onLeave)
            <> (layoutResult.rowLabels <#> renderAdjacencyLabel hovered onHover onLeave)
            <> (layoutResult.colLabels <#> renderAdjacencyLabel hovered onHover onLeave)
          )
      ]

renderAdjacencyCell
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Adjacency.MatrixCell
  -> HH.HTML w i
renderAdjacencyCell hovered onHover onLeave cell =
  let
    isHighlighted = case hovered of
      Just h -> h == cell.rowName || h == cell.colName
      Nothing -> false
    -- Color intensity based on value
    intensity = min 1.0 (cell.value / 50.0)  -- Normalize
    baseColor = if isHighlighted
      then colorBrewerSet2 0
      else mutedFlow 0
    fillOpacity = if cell.value > 0.0
      then fmt (0.2 + intensity * 0.8)
      else "0"
  in
    rect
      [ HP.attr (HH.AttrName "x") (fmt cell.position.x)
      , HP.attr (HH.AttrName "y") (fmt cell.position.y)
      , HP.attr (HH.AttrName "width") (fmt cell.position.width)
      , HP.attr (HH.AttrName "height") (fmt cell.position.height)
      , HP.attr (HH.AttrName "fill") baseColor
      , HP.attr (HH.AttrName "fill-opacity") fillOpacity
      , HP.attr (HH.AttrName "stroke") "#d4c9b8"
      , HP.attr (HH.AttrName "stroke-width") "0.5"
      , HP.attr (HH.AttrName "class") "adjacency-cell"
      , HE.onMouseEnter \_ -> onHover cell.rowName
      , HE.onMouseLeave \_ -> onLeave
      ]

renderAdjacencyLabel
  :: forall w i
   . Maybe String
  -> (String -> i)
  -> i
  -> Adjacency.MatrixLabel
  -> HH.HTML w i
renderAdjacencyLabel hovered onHover onLeave label =
  let
    isHighlighted = hovered == Just label.name
    fillColor = if isHighlighted then colorInkBrown else colorInkSepia
    fontWeight = if isHighlighted then "bold" else "normal"
    transform = if label.position.rotation /= 0.0
      then "rotate(" <> fmt label.position.rotation <> " " <> fmt label.position.x <> " " <> fmt label.position.y <> ")"
      else ""
  in
    text
      [ HP.attr (HH.AttrName "x") (fmt label.position.x)
      , HP.attr (HH.AttrName "y") (fmt label.position.y)
      , HP.attr (HH.AttrName "text-anchor") label.position.anchor
      , HP.attr (HH.AttrName "font-size") "8"
      , HP.attr (HH.AttrName "fill") fillColor
      , HP.attr (HH.AttrName "font-weight") fontWeight
      , HP.attr (HH.AttrName "transform") transform
      , HP.attr (HH.AttrName "class") "adjacency-label"
      , HE.onMouseEnter \_ -> onHover label.name
      , HE.onMouseLeave \_ -> onLeave
      ]
      [ HH.text label.displayName ]

-- | SVG text element helper
text :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
text props children = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "text") props children

-- Additional color constants for flow layouts
colorInkBrown :: String
colorInkBrown = "#3d2b1f"
