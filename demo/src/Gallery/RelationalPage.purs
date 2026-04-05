-- | Relational Page
-- |
-- | Shows 3 relational/connection layouts (Chord, EdgeBundle, Adjacency)
-- | in circular viewports with HATS coordinated highlighting.
module Gallery.RelationalPage where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Gallery.FlowData (MatrixData, EdgeBundleData, loadMatrixData, loadEdgeBundleData)
import Gallery.RenderHATS as HATS
import Gallery.Types (LayoutType(..), layoutInfo)

-- =============================================================================
-- Types
-- =============================================================================

type RelationalData =
  { matrix :: Maybe MatrixData
  , edgeBundle :: Maybe EdgeBundleData
  }

type State =
  { relData :: RelationalData
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | MatrixLoaded (Either String MatrixData)
  | EdgeBundleLoaded (Either String EdgeBundleData)
  | RenderLayouts

relationalLayouts :: Array LayoutType
relationalLayouts = [ Chord, EdgeBundle, Adjacency ]

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
  { relData: { matrix: Nothing, edgeBundle: Nothing }
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
        [ HP.class_ (H.ClassName "gallery-grid flow-grid") ]
        (relationalLayouts <#> renderLayoutCard)
    , renderFooter
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Relational Layouts" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "3 layouts for relational data \x2014 hover any element for coordinated highlighting" ]
    , HH.p
        [ HP.class_ (H.ClassName "gallery-nav") ]
        [ HH.a [ HP.href "#" ] [ HH.text "\x2190 Gallery" ]
        , HH.text " \x00b7 "
        , HH.a [ HP.href "#flow" ] [ HH.text "Flow" ]
        , HH.text " \x00b7 "
        , HH.a [ HP.href "#hierarchy" ] [ HH.text "Hierarchy" ]
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
layoutTypeAttr Chord = "chord"
layoutTypeAttr EdgeBundle = "edge-bundle"
layoutTypeAttr Adjacency = "adjacency"
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
renderHATSLayouts state = do
  case state.relData.matrix of
    Nothing -> pure unit
    Just matrixData -> do
      HATS.renderChord ("#" <> hatsContainerId Chord) matrixData
      HATS.renderAdjacency ("#" <> hatsContainerId Adjacency) matrixData

  case state.relData.edgeBundle of
    Nothing -> pure unit
    Just bundleData ->
      HATS.renderEdgeBundle ("#" <> hatsContainerId EdgeBundle) bundleData

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    matrixResult <- liftAff loadMatrixData
    handleAction (MatrixLoaded matrixResult)
    edgeBundleResult <- liftAff loadEdgeBundleData
    handleAction (EdgeBundleLoaded edgeBundleResult)

  MatrixLoaded result -> do
    case result of
      Left _ -> pure unit
      Right matrixData -> do
        H.modify_ \s -> s { relData = s.relData { matrix = Just matrixData }, loading = false }
        handleAction RenderLayouts

  EdgeBundleLoaded result -> do
    case result of
      Left _ -> pure unit
      Right edgeBundleData -> do
        H.modify_ \s -> s { relData = s.relData { edgeBundle = Just edgeBundleData } }
        handleAction RenderLayouts

  RenderLayouts -> do
    state <- H.get
    liftEffect $ renderHATSLayouts state
