-- | Hierarchy Page
-- |
-- | Shows 10 hierarchy layouts (Tree×3, Cluster×3, Pack, Sunburst, Icicle, Treemap)
-- | in circular viewports with HATS coordinated highlighting.
module Gallery.HierarchyPage where

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
import Gallery.RenderHATS as HATS
import Gallery.Types (LayoutType(..), layoutInfo)

-- =============================================================================
-- Types
-- =============================================================================

type HierarchyData =
  { tree :: Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  , pack :: Pack.HierarchyData { name :: String, path :: String }
  , partition :: Partition.HierarchyData { name :: String, path :: String }
  }

type State =
  { flareData :: Maybe HierarchyData
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | DataLoaded (Either String FlareNode)
  | RenderLayouts

-- | Hierarchy layouts in display order: 4-3-3
hierarchyLayouts :: Array LayoutType
hierarchyLayouts =
  [ TreeHorizontal, Pack, TreeVertical, PartitionSunburst
  , TreeRadial, PartitionIcicle, ClusterHorizontal
  , Treemap, ClusterVertical, ClusterRadial
  ]

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
  { flareData: Nothing
  , loading: true
  , error: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (H.ClassName "gallery-container") ]
    [ renderHeader
    , HH.div
        [ HP.class_ (H.ClassName "gallery-grid hierarchy-grid") ]
        (hierarchyLayouts <#> renderLayoutCard)
    , renderFooter
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Hierarchy Layouts" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "10 layouts for nested data \x2014 hover any element for coordinated highlighting" ]
    , HH.p
        [ HP.class_ (H.ClassName "gallery-nav") ]
        [ HH.a [ HP.href "#" ] [ HH.text "\x2190 Gallery" ]
        , HH.text " \x00b7 "
        , HH.a [ HP.href "#flow" ] [ HH.text "Flow" ]
        , HH.text " \x00b7 "
        , HH.a [ HP.href "#pattern" ] [ HH.text "Pattern" ]
        ]
    ]

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
layoutTypeAttr _ = ""

renderFooter :: forall m. H.ComponentHTML Action () m
renderFooter =
  HH.div
    [ HP.class_ (H.ClassName "gallery-footer") ]
    [ HH.p_
        [ HH.text "From the "
        , HH.a
            [ HP.href "https://github.com/afcondon/purescript-d3-layout" ]
            [ HH.text "hylograph-layout" ]
        , HH.text " library"
        ]
    ]

-- =============================================================================
-- HATS Rendering
-- =============================================================================

renderHATSLayouts :: State -> Effect Unit
renderHATSLayouts state =
  case state.flareData of
    Nothing -> pure unit
    Just hierData -> do
      HATS.renderTreeHorizontal ("#" <> hatsContainerId TreeHorizontal) hierData.tree
      HATS.renderTreeVertical ("#" <> hatsContainerId TreeVertical) hierData.tree
      HATS.renderTreeRadial ("#" <> hatsContainerId TreeRadial) hierData.tree
      HATS.renderClusterHorizontal ("#" <> hatsContainerId ClusterHorizontal) hierData.tree
      HATS.renderClusterVertical ("#" <> hatsContainerId ClusterVertical) hierData.tree
      HATS.renderClusterRadial ("#" <> hatsContainerId ClusterRadial) hierData.tree
      HATS.renderPack ("#" <> hatsContainerId Pack) hierData.pack
      HATS.renderSunburst ("#" <> hatsContainerId PartitionSunburst) hierData.partition
      HATS.renderIcicle ("#" <> hatsContainerId PartitionIcicle) hierData.partition
      HATS.renderTreemap ("#" <> hatsContainerId Treemap) hierData.partition

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    flareResult <- liftAff loadFlareData
    handleAction (DataLoaded flareResult)

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
        handleAction RenderLayouts

  RenderLayouts -> do
    state <- H.get
    liftEffect $ renderHATSLayouts state
