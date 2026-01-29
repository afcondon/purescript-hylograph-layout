-- | Layout Gallery - Main Halogen Component
-- |
-- | Showcases 14 Hylograph layout algorithms in circular viewports
-- | Illuminated manuscript aesthetic with offset grid layout
-- | Coordinated highlighting across all views
module Gallery.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tree (Tree)
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Gallery.Data (FlareNode, loadFlareData, flareToTree, flareToPackHierarchy, flareToPartitionHierarchy)
import Gallery.FlowData (SankeyData, MatrixData, EdgeBundleData, loadSankeyData, loadMatrixData, loadEdgeBundleData)
import Gallery.Render as Render
import Gallery.Types (LayoutType(..), allLayouts, layoutInfo)

-- =============================================================================
-- Types
-- =============================================================================

-- | Loaded hierarchical data in various formats for different layouts
-- | All include path field for coordinated highlighting
type HierarchyData =
  { tree :: Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  , pack :: Pack.HierarchyData { name :: String, path :: String }
  , partition :: Partition.HierarchyData { name :: String, path :: String }
  }

-- | Flow data for non-hierarchical layouts
type FlowData =
  { sankey :: Maybe SankeyData
  , matrix :: Maybe MatrixData      -- Used by both Chord and Adjacency
  , edgeBundle :: Maybe EdgeBundleData
  }

-- | Which data group is being hovered (for scoped coordination)
data HoverGroup
  = HoverHierarchy    -- Tree, Cluster, Pack, Partition, Treemap (flare data)
  | HoverSankey       -- Sankey (energy data)
  | HoverMatrix       -- Chord, Adjacency (bridges data)
  | HoverEdgeBundle   -- EdgeBundle (concept-graph data)

derive instance eqHoverGroup :: Eq HoverGroup

type State =
  { layouts :: Array LayoutType
  , flareData :: Maybe HierarchyData
  , flowData :: FlowData
  , loading :: Boolean
  , error :: Maybe String
  , hoveredPath :: Maybe String  -- Currently hovered node path
  , hoveredGroup :: Maybe HoverGroup  -- Which data group is being hovered
  }

data Action
  = Initialize
  | DataLoaded (Either String FlareNode)
  | SankeyLoaded (Either String SankeyData)
  | MatrixLoaded (Either String MatrixData)
  | EdgeBundleLoaded (Either String EdgeBundleData)
  | HoverNode HoverGroup String  -- Mouse entered a node (with group)
  | UnhoverNode                  -- Mouse left a node

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: forall input. input -> State
initialState _ =
  { layouts: allLayouts
  , flareData: Nothing
  , flowData: { sankey: Nothing, matrix: Nothing, edgeBundle: Nothing }
  , loading: true
  , error: Nothing
  , hoveredPath: Nothing
  , hoveredGroup: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "gallery-container") ]
    [ renderHeader
    , renderGrid state
    , renderFooter
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Layout Gallery" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "14 100% PureScript layout algorithms — hierarchies and flow diagrams" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.text "(hover any element in the hierarchical layouts to see coordinated highlighting)" ]
    ]

renderGrid :: forall m. State -> H.ComponentHTML Action () m
renderGrid state =
  HH.div
    [ HP.class_ (H.ClassName "gallery-grid") ]
    (state.layouts <#> renderLayoutCard state)

renderLayoutCard :: forall m. State -> LayoutType -> H.ComponentHTML Action () m
renderLayoutCard state layoutType =
  HH.div
    [ HP.class_ (H.ClassName "layout-card")
    , HP.attr (HH.AttrName "data-layout") (layoutTypeAttr layoutType)
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "circle-viewport") ]
        [ renderLayoutContent state layoutType ]
    , HH.div
        [ HP.class_ (H.ClassName "layout-label") ]
        [ HH.h3_ [ HH.text info.name ]
        , HH.p_ [ HH.text info.description ]
        ]
    ]
  where
  info = layoutInfo layoutType

-- | Render the actual layout content based on available data
renderLayoutContent :: forall m. State -> LayoutType -> H.ComponentHTML Action () m
renderLayoutContent state layoutType =
  let
    -- Only show hover state if we're in the matching group
    hoveredFor group = case state.hoveredGroup of
      Just g | g == group -> state.hoveredPath
      _ -> Nothing
    onLeave = UnhoverNode
  in
    case layoutType of
      -- Hierarchical layouts - need flare data, coordinate within hierarchy group
      TreeHorizontal -> maybe (Render.renderPlaceholder layoutType) (Render.renderTreeHorizontal (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.tree) state.flareData
      TreeVertical -> maybe (Render.renderPlaceholder layoutType) (Render.renderTreeVertical (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.tree) state.flareData
      TreeRadial -> maybe (Render.renderPlaceholder layoutType) (Render.renderTreeRadial (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.tree) state.flareData
      ClusterHorizontal -> maybe (Render.renderPlaceholder layoutType) (Render.renderClusterHorizontal (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.tree) state.flareData
      ClusterVertical -> maybe (Render.renderPlaceholder layoutType) (Render.renderClusterVertical (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.tree) state.flareData
      ClusterRadial -> maybe (Render.renderPlaceholder layoutType) (Render.renderClusterRadial (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.tree) state.flareData
      Pack -> maybe (Render.renderPlaceholder layoutType) (Render.renderPack (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.pack) state.flareData
      PartitionSunburst -> maybe (Render.renderPlaceholder layoutType) (Render.renderSunburst (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.partition) state.flareData
      PartitionIcicle -> maybe (Render.renderPlaceholder layoutType) (Render.renderIcicle (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.partition) state.flareData
      Treemap -> maybe (Render.renderPlaceholder layoutType) (Render.renderTreemap (hoveredFor HoverHierarchy) (HoverNode HoverHierarchy) onLeave <<< _.partition) state.flareData
      -- Flow layouts - each has its own coordination group
      Sankey -> maybe (Render.renderPlaceholder layoutType) (Render.renderSankey (hoveredFor HoverSankey) (HoverNode HoverSankey) onLeave) state.flowData.sankey
      Chord -> maybe (Render.renderPlaceholder layoutType) (Render.renderChord (hoveredFor HoverMatrix) (HoverNode HoverMatrix) onLeave) state.flowData.matrix
      EdgeBundle -> maybe (Render.renderPlaceholder layoutType) (Render.renderEdgeBundle (hoveredFor HoverEdgeBundle) (HoverNode HoverEdgeBundle) onLeave) state.flowData.edgeBundle
      Adjacency -> maybe (Render.renderPlaceholder layoutType) (Render.renderAdjacency (hoveredFor HoverMatrix) (HoverNode HoverMatrix) onLeave) state.flowData.matrix

layoutTypeAttr :: LayoutType -> String
layoutTypeAttr TreeHorizontal = "tree-horizontal"
layoutTypeAttr TreeVertical = "tree-vertical"
layoutTypeAttr TreeRadial = "tree-radial"
layoutTypeAttr ClusterHorizontal = "cluster-horizontal"
layoutTypeAttr ClusterVertical = "cluster-vertical"
layoutTypeAttr ClusterRadial = "cluster-radial"
layoutTypeAttr Pack = "pack"
layoutTypeAttr PartitionSunburst = "partition-sunburst"
layoutTypeAttr PartitionIcicle = "partition-icicle"
layoutTypeAttr Treemap = "treemap"
layoutTypeAttr Chord = "chord"
layoutTypeAttr Sankey = "sankey"
layoutTypeAttr EdgeBundle = "edge-bundle"
layoutTypeAttr Adjacency = "adjacency"

renderFooter :: forall m. H.ComponentHTML Action () m
renderFooter =
  HH.div
    [ HP.class_ (H.ClassName "gallery-footer") ]
    [ HH.p_
        [ HH.text "From the "
        , HH.a
            [ HP.href "https://github.com/afcondon/purescript-d3-layout" ]
            [ HH.text "hylograph-layout" ]
        , HH.text " library — Pure PureScript"
        ]
    ]

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Load hierarchy data
    flareResult <- liftAff loadFlareData
    handleAction (DataLoaded flareResult)
    -- Load flow data in parallel
    sankeyResult <- liftAff loadSankeyData
    handleAction (SankeyLoaded sankeyResult)
    matrixResult <- liftAff loadMatrixData
    handleAction (MatrixLoaded matrixResult)
    edgeBundleResult <- liftAff loadEdgeBundleData
    handleAction (EdgeBundleLoaded edgeBundleResult)

  DataLoaded result -> do
    case result of
      Left err ->
        H.modify_ _ { loading = false, error = Just err }
      Right flareNode ->
        let
          hierData =
            { tree: flareToTree flareNode
            , pack: flareToPackHierarchy flareNode
            , partition: flareToPartitionHierarchy flareNode
            }
        in
          H.modify_ _ { loading = false, flareData = Just hierData }

  SankeyLoaded result -> do
    case result of
      Left _ -> pure unit  -- Silently ignore flow data errors
      Right sankeyData ->
        H.modify_ \s -> s { flowData = s.flowData { sankey = Just sankeyData } }

  MatrixLoaded result -> do
    case result of
      Left _ -> pure unit
      Right matrixData ->
        H.modify_ \s -> s { flowData = s.flowData { matrix = Just matrixData } }

  EdgeBundleLoaded result -> do
    case result of
      Left _ -> pure unit
      Right edgeBundleData ->
        H.modify_ \s -> s { flowData = s.flowData { edgeBundle = Just edgeBundleData } }

  HoverNode group path -> do
    H.modify_ _ { hoveredPath = Just path, hoveredGroup = Just group }

  UnhoverNode -> do
    H.modify_ _ { hoveredPath = Nothing, hoveredGroup = Nothing }
