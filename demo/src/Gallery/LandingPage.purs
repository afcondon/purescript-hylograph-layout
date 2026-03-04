-- | Landing Page
-- |
-- | Navigation page with 3 category cards, each with a representative
-- | circular viewport preview: Tree (hierarchy), Chord (flow), Swimlane (pattern).
module Gallery.LandingPage where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tree (Tree)
import DataViz.Layout.Pattern (swimlane)
import DataViz.Layout.Pattern.Types (Rect, viewport)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Gallery.Data (FlareNode, loadFlareData, flareToTree)
import Gallery.FlowData (MatrixData, loadMatrixData)
import Gallery.RenderHATS as HATS

-- =============================================================================
-- Types
-- =============================================================================

type HierNode =
  { name :: String, path :: String, value :: Number
  , x :: Number, y :: Number, depth :: Int, height :: Int
  }

type State =
  { treeData :: Maybe (Tree HierNode)
  , matrixData :: Maybe MatrixData
  , swimlaneRects :: Array Rect
  }

data Action
  = Initialize
  | FlareLoaded (Either String FlareNode)
  | MatrixLoaded (Either String MatrixData)
  | RenderPreviews

-- Static swimlane items for the preview
swimlaneItems :: Array { lane :: Int, start :: Number, end :: Number }
swimlaneItems =
  [ { lane: 0, start: 0.05, end: 0.35 }
  , { lane: 1, start: 0.10, end: 0.45 }
  , { lane: 2, start: 0.00, end: 0.25 }
  , { lane: 3, start: 0.20, end: 0.55 }
  , { lane: 0, start: 0.40, end: 0.70 }
  , { lane: 1, start: 0.50, end: 0.80 }
  , { lane: 2, start: 0.30, end: 0.60 }
  , { lane: 3, start: 0.60, end: 0.90 }
  , { lane: 0, start: 0.75, end: 0.95 }
  , { lane: 2, start: 0.65, end: 0.85 }
  , { lane: 1, start: 0.85, end: 1.00 }
  , { lane: 3, start: 0.92, end: 1.00 }
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
  { treeData: Nothing
  , matrixData: Nothing
  , swimlaneRects: swimlane 4 6.0 (viewport 400.0 400.0) swimlaneItems
  }

-- =============================================================================
-- Render
-- =============================================================================

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgElem :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "svg")

rectElem :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
rectElem props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "rect") props []

palette :: Array String
palette =
  [ "#c23b22", "#1e3a5f", "#c9a227", "#2d5a27"
  , "#66023c", "#cc7722", "#0d6e6e", "#5c4033"
  ]

colorAt :: Int -> String
colorAt i = case Array.index palette (i `mod` Array.length palette) of
  Just c -> c
  Nothing -> "#999"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "landing-container") ]
    [ renderHeader
    , HH.div
        [ HP.class_ (H.ClassName "landing-cards") ]
        [ hierarchyCard
        , flowCard
        , patternCard state.swimlaneRects
        ]
    , renderFooter
    ]

hierarchyCard :: forall m. H.ComponentHTML Action () m
hierarchyCard =
  HH.a
    [ HP.href "#hierarchy"
    , HP.class_ (H.ClassName "landing-card")
    , HP.attr (HH.AttrName "style") "--card-accent: var(--forest-green)"
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "landing-card-preview") ]
        [ HH.div
            [ HP.class_ (H.ClassName "circle-viewport landing-viewport")
            , HP.id "landing-tree"
            ]
            []
        ]
    , HH.div
        [ HP.class_ (H.ClassName "landing-card-body") ]
        [ HH.h2_ [ HH.text "Hierarchy" ]
        , HH.p [ HP.class_ (H.ClassName "landing-card-desc") ]
            [ HH.text "Tree, cluster, pack, sunburst, icicle, and treemap layouts for nested data" ]
        , HH.p [ HP.class_ (H.ClassName "landing-card-count") ]
            [ HH.text "10 layouts" ]
        ]
    ]

flowCard :: forall m. H.ComponentHTML Action () m
flowCard =
  HH.a
    [ HP.href "#flow"
    , HP.class_ (H.ClassName "landing-card")
    , HP.attr (HH.AttrName "style") "--card-accent: var(--ultramarine)"
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "landing-card-preview") ]
        [ HH.div
            [ HP.class_ (H.ClassName "circle-viewport landing-viewport")
            , HP.id "landing-chord"
            ]
            []
        ]
    , HH.div
        [ HP.class_ (H.ClassName "landing-card-body") ]
        [ HH.h2_ [ HH.text "Flow" ]
        , HH.p [ HP.class_ (H.ClassName "landing-card-desc") ]
            [ HH.text "Sankey, chord, edge bundle, and adjacency layouts for relational data" ]
        , HH.p [ HP.class_ (H.ClassName "landing-card-count") ]
            [ HH.text "4 layouts" ]
        ]
    ]

patternCard :: forall m. Array Rect -> H.ComponentHTML Action () m
patternCard rects =
  HH.a
    [ HP.href "#pattern"
    , HP.class_ (H.ClassName "landing-card")
    , HP.attr (HH.AttrName "style") "--card-accent: var(--vermillion)"
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "landing-card-preview") ]
        [ HH.div
            [ HP.class_ (H.ClassName "circle-viewport landing-viewport") ]
            [ svgElem
                [ HP.attr (HH.AttrName "viewBox") "0 0 400 400"
                , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
                , HP.attr (HH.AttrName "width") "100%"
                , HP.attr (HH.AttrName "height") "100%"
                ]
                (Array.mapWithIndex renderSwimlaneRect rects)
            ]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "landing-card-body") ]
        [ HH.h2_ [ HH.text "Pattern" ]
        , HH.p [ HP.class_ (H.ClassName "landing-card-desc") ]
            [ HH.text "Masonry, shelf, waffle, stacked, waterfall, justified, bin pack, calendar, and swimlane" ]
        , HH.p [ HP.class_ (H.ClassName "landing-card-count") ]
            [ HH.text "9 layouts" ]
        ]
    ]

renderSwimlaneRect :: forall w i. Int -> Rect -> HH.HTML w i
renderSwimlaneRect i r =
  rectElem
    [ HP.attr (HH.AttrName "x") (show r.x)
    , HP.attr (HH.AttrName "y") (show r.y)
    , HP.attr (HH.AttrName "width") (show r.width)
    , HP.attr (HH.AttrName "height") (show r.height)
    , HP.attr (HH.AttrName "rx") "4"
    , HP.attr (HH.AttrName "ry") "4"
    , HP.attr (HH.AttrName "fill") (colorAt i)
    , HP.attr (HH.AttrName "opacity") "0.85"
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Layout Gallery" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "23 pure PureScript layout algorithms" ]
    ]

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

renderPreviews :: State -> Effect Unit
renderPreviews state = do
  case state.treeData of
    Nothing -> pure unit
    Just tree -> HATS.renderTreeHorizontal "#landing-tree" tree
  case state.matrixData of
    Nothing -> pure unit
    Just matrixData -> HATS.renderChord "#landing-chord" matrixData

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    flareResult <- liftAff loadFlareData
    handleAction (FlareLoaded flareResult)
    matrixResult <- liftAff loadMatrixData
    handleAction (MatrixLoaded matrixResult)

  FlareLoaded result -> do
    case result of
      Left _ -> pure unit
      Right flareNode -> do
        H.modify_ _ { treeData = Just (flareToTree flareNode) }
        handleAction RenderPreviews

  MatrixLoaded result -> do
    case result of
      Left _ -> pure unit
      Right matrixData -> do
        H.modify_ _ { matrixData = Just matrixData }
        handleAction RenderPreviews

  RenderPreviews -> do
    state <- H.get
    liftEffect $ renderPreviews state
