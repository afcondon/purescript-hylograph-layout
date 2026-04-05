-- | Flow Page
-- |
-- | Shows Sankey/ribbon flow diagrams: acyclic and end-cyclic.
-- | Full-width, full-color diagrams with HATS coordinated highlighting.
module Gallery.FlowPage where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import DataViz.Layout.Sankey.Types (LinkCSVRow)
import Gallery.FlowData (SankeyData, loadSankeyData)
import Gallery.RenderHATS as HATS

-- =============================================================================
-- Datasets
-- =============================================================================

-- | End cycle: circular economy (output feeds back to input)
endCycleData :: Array LinkCSVRow
endCycleData =
  [ { s: "Raw Material", t: "Manufacturing", v: 50.0 }
  , { s: "Manufacturing", t: "Distribution", v: 45.0 }
  , { s: "Manufacturing", t: "Scrap", v: 5.0 }
  , { s: "Distribution", t: "Consumer", v: 40.0 }
  , { s: "Distribution", t: "Waste", v: 5.0 }
  , { s: "Consumer", t: "Disposal", v: 15.0 }
  , { s: "Consumer", t: "Collection", v: 25.0 }
  , { s: "Collection", t: "Recycling", v: 20.0 }
  , { s: "Collection", t: "Waste", v: 5.0 }
  , { s: "Recycling", t: "Raw Material", v: 18.0 }
  , { s: "Scrap", t: "Recycling", v: 4.0 }
  ]

-- =============================================================================
-- Types
-- =============================================================================

type State =
  { sankeyData :: Maybe SankeyData
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | SankeyLoaded (Either String SankeyData)
  | RenderLayouts

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
  { sankeyData: Nothing
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
        [ HP.class_ (H.ClassName "ribbon-grid") ]
        [ renderAcyclicCard
        , renderEndCyclicCard
        , renderGeneralizedNote
        ]
    , renderFooter
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Flow Layouts" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Sankey and ribbon diagrams \x2014 acyclic and cyclic flow" ]
    , HH.p
        [ HP.class_ (H.ClassName "gallery-nav") ]
        [ HH.a [ HP.href "#" ] [ HH.text "\x2190 Gallery" ]
        , HH.text " \x00b7 "
        , HH.a [ HP.href "#relational" ] [ HH.text "Relational" ]
        , HH.text " \x00b7 "
        , HH.a [ HP.href "#hierarchy" ] [ HH.text "Hierarchy" ]
        , HH.text " \x00b7 "
        , HH.a [ HP.href "#pattern" ] [ HH.text "Pattern" ]
        ]
    ]

renderAcyclicCard :: forall m. H.ComponentHTML Action () m
renderAcyclicCard =
  HH.div
    [ HP.class_ (H.ClassName "ribbon-card") ]
    [ HH.div
        [ HP.class_ (H.ClassName "ribbon-card-header") ]
        [ HH.h3_ [ HH.text "Sankey Diagram" ]
        , HH.span
            [ HP.class_ (H.ClassName "ribbon-topology-badge topology-Acyclic") ]
            [ HH.text "Acyclic" ]
        ]
    , HH.p
        [ HP.class_ (H.ClassName "ribbon-card-description") ]
        [ HH.text "Energy flows from source to consumption. Conservation of flow at each node." ]
    , HH.div
        [ HP.class_ (H.ClassName "ribbon-viewport")
        , HP.id "flow-acyclic"
        ]
        []
    ]

renderEndCyclicCard :: forall m. H.ComponentHTML Action () m
renderEndCyclicCard =
  HH.div
    [ HP.class_ (H.ClassName "ribbon-card") ]
    [ HH.div
        [ HP.class_ (H.ClassName "ribbon-card-header") ]
        [ HH.h3_ [ HH.text "Looping Sankey" ]
        , HH.span
            [ HP.class_ (H.ClassName "ribbon-topology-badge topology-EndCyclic") ]
            [ HH.text "EndCyclic" ]
        ]
    , HH.p
        [ HP.class_ (H.ClassName "ribbon-card-description") ]
        [ HH.text "Circular economy \x2014 recycled material flows back to raw input. Faded copies show the repeating cycle." ]
    , HH.div
        [ HP.class_ (H.ClassName "ribbon-viewport")
        , HP.id "flow-end-cyclic"
        ]
        []
    ]

-- | Note about the generalized flow diagram capability
renderGeneralizedNote :: forall m. H.ComponentHTML Action () m
renderGeneralizedNote =
  HH.div
    [ HP.class_ (H.ClassName "ribbon-note") ]
    [ HH.p_
        [ HH.text "The layout engine behind these diagrams is generalized beyond Sankey. The node sizing function is a parameter \x2014 any fold over incident flows \x2014 not hardcoded to conservation-of-flow. We're looking for a compelling real-world dataset that naturally calls for different flow semantics. "
        , HH.a
            [ HP.href "https://github.com/afcondon/purescript-hylograph-layout/issues" ]
            [ HH.text "Get in touch" ]
        , HH.text " if you have one."
        ]
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
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    sankeyResult <- liftAff loadSankeyData
    handleAction (SankeyLoaded sankeyResult)

  SankeyLoaded result -> do
    case result of
      Left _ -> pure unit
      Right sankeyData -> do
        H.modify_ _ { sankeyData = Just sankeyData, loading = false }
        handleAction RenderLayouts

  RenderLayouts -> do
    state <- H.get
    liftEffect $ renderFlowLayouts state

renderFlowLayouts :: State -> Effect Unit
renderFlowLayouts state = do
  case state.sankeyData of
    Nothing -> pure unit
    Just sankeyData ->
      HATS.renderSankeyWide "#flow-acyclic" sankeyData.links 900.0 350.0

  HATS.renderSankeyWide "#flow-end-cyclic" endCycleData 900.0 350.0
