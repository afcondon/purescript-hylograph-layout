-- | HATS-powered Layout Gallery Component
-- |
-- | Uses HATS for rendering ALL layouts with coordinated highlighting.
-- | Highlighting is fully automatic via HATS behaviors - no Halogen state needed.
module Gallery.ComponentHATS where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tree (Tree)
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Gallery.Data (FlareNode, loadFlareData, flareToTree, flareToPackHierarchy, flareToPartitionHierarchy)
import Gallery.FlowData (SankeyData, MatrixData, EdgeBundleData, loadSankeyData, loadMatrixData, loadEdgeBundleData)
import Gallery.RenderHATS as HATS
import Gallery.Types (LayoutType(..), allLayouts, layoutInfo)

-- =============================================================================
-- Types
-- =============================================================================

-- | Loaded hierarchical data in various formats for different layouts
type HierarchyData =
  { tree :: Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  , pack :: Pack.HierarchyData { name :: String, path :: String }
  , partition :: Partition.HierarchyData { name :: String, path :: String }
  }

-- | Flow data for non-hierarchical layouts
type FlowData =
  { sankey :: Maybe SankeyData
  , matrix :: Maybe MatrixData
  , edgeBundle :: Maybe EdgeBundleData
  }

-- | Component state - simplified since HATS handles highlighting automatically
type State =
  { layouts :: Array LayoutType
  , flareData :: Maybe HierarchyData
  , flowData :: FlowData
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | DataLoaded (Either String FlareNode)
  | SankeyLoaded (Either String SankeyData)
  | MatrixLoaded (Either String MatrixData)
  | EdgeBundleLoaded (Either String EdgeBundleData)
  | RenderHATSLayouts

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
    [ HH.h1_ [ HH.text "Layout Gallery (HATS)" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "14 100% PureScript layout algorithms — fully HATS-rendered" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.text "(hover any element to see coordinated highlighting)" ]
    ]

renderGrid :: forall m. State -> H.ComponentHTML Action () m
renderGrid state =
  HH.div
    [ HP.class_ (H.ClassName "gallery-grid") ]
    (state.layouts <#> renderLayoutCard)

renderLayoutCard :: forall m. LayoutType -> H.ComponentHTML Action () m
renderLayoutCard layoutType =
  HH.div
    [ HP.class_ (H.ClassName "layout-card")
    , HP.attr (HH.AttrName "data-layout") (layoutTypeAttr layoutType)
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "circle-viewport")
        , HP.id (hatsContainerId layoutType)
        ]
        -- HATS renders into this container imperatively
        []
    , HH.div
        [ HP.class_ (H.ClassName "layout-label") ]
        [ HH.h3_ [ HH.text info.name ]
        , HH.p_ [ HH.text info.description ]
        ]
    ]
  where
  info = layoutInfo layoutType

hatsContainerId :: LayoutType -> String
hatsContainerId layoutType = "hats-" <> layoutTypeAttr layoutType

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
        , HH.text " library — Pure PureScript + HATS"
        ]
    ]

-- =============================================================================
-- HATS Rendering
-- =============================================================================

-- | Render all HATS layouts - highlighting is handled automatically by HATS behaviors
renderHATSLayouts :: State -> Effect Unit
renderHATSLayouts state = do
  -- Render hierarchy layouts
  case state.flareData of
    Nothing -> pure unit
    Just hierData -> do
      -- Tree layouts
      HATS.renderTreeHorizontal ("#" <> hatsContainerId TreeHorizontal) hierData.tree
      HATS.renderTreeVertical ("#" <> hatsContainerId TreeVertical) hierData.tree
      HATS.renderTreeRadial ("#" <> hatsContainerId TreeRadial) hierData.tree

      -- Cluster layouts
      HATS.renderClusterHorizontal ("#" <> hatsContainerId ClusterHorizontal) hierData.tree
      HATS.renderClusterVertical ("#" <> hatsContainerId ClusterVertical) hierData.tree
      HATS.renderClusterRadial ("#" <> hatsContainerId ClusterRadial) hierData.tree

      -- Pack layout
      HATS.renderPack ("#" <> hatsContainerId Pack) hierData.pack

      -- Partition layouts
      HATS.renderSunburst ("#" <> hatsContainerId PartitionSunburst) hierData.partition
      HATS.renderIcicle ("#" <> hatsContainerId PartitionIcicle) hierData.partition
      HATS.renderTreemap ("#" <> hatsContainerId Treemap) hierData.partition

  -- Render Sankey
  case state.flowData.sankey of
    Nothing -> pure unit
    Just sankeyData ->
      HATS.renderSankey ("#" <> hatsContainerId Sankey) sankeyData

  -- Render Chord and Adjacency (both use matrix data)
  case state.flowData.matrix of
    Nothing -> pure unit
    Just matrixData -> do
      HATS.renderChord ("#" <> hatsContainerId Chord) matrixData
      HATS.renderAdjacency ("#" <> hatsContainerId Adjacency) matrixData

  -- Render Edge Bundle
  case state.flowData.edgeBundle of
    Nothing -> pure unit
    Just bundleData ->
      HATS.renderEdgeBundle ("#" <> hatsContainerId EdgeBundle) bundleData

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Load hierarchy data
    flareResult <- liftAff loadFlareData
    handleAction (DataLoaded flareResult)

    -- Load flow data
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
      Right flareNode -> do
        let
          hierData =
            { tree: flareToTree flareNode
            , pack: flareToPackHierarchy flareNode
            , partition: flareToPartitionHierarchy flareNode
            }
        H.modify_ _ { loading = false, flareData = Just hierData }
        handleAction RenderHATSLayouts

  SankeyLoaded result -> do
    case result of
      Left _ -> pure unit
      Right sankeyData -> do
        H.modify_ \s -> s { flowData = s.flowData { sankey = Just sankeyData } }
        handleAction RenderHATSLayouts

  MatrixLoaded result -> do
    case result of
      Left _ -> pure unit
      Right matrixData -> do
        H.modify_ \s -> s { flowData = s.flowData { matrix = Just matrixData } }
        handleAction RenderHATSLayouts

  EdgeBundleLoaded result -> do
    case result of
      Left _ -> pure unit
      Right edgeBundleData -> do
        H.modify_ \s -> s { flowData = s.flowData { edgeBundle = Just edgeBundleData } }
        handleAction RenderHATSLayouts

  RenderHATSLayouts -> do
    state <- H.get
    liftEffect $ renderHATSLayouts state
