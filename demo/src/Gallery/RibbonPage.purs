-- | Ribbon Page
-- |
-- | Shows 4 flow diagrams illustrating cycle topology variants:
-- | Acyclic, EndCyclic, InteriorCyclic, MixedCyclic.
-- | Full-width Sankey diagrams stacked vertically.
module Gallery.RibbonPage where

import Prelude

import Data.Maybe (Maybe(..))
import DataViz.Layout.Sankey.Compute as Sankey
import DataViz.Layout.Sankey.Types (LinkCSVRow)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Gallery.RenderHATS as HATS

-- =============================================================================
-- Datasets
-- =============================================================================

-- | Acyclic: an energy-style flow network
acyclicData :: Array LinkCSVRow
acyclicData =
  [ { s: "Solar", t: "Grid", v: 40.0 }
  , { s: "Wind", t: "Grid", v: 35.0 }
  , { s: "Gas", t: "Grid", v: 25.0 }
  , { s: "Grid", t: "Residential", v: 45.0 }
  , { s: "Grid", t: "Commercial", v: 30.0 }
  , { s: "Grid", t: "Industrial", v: 20.0 }
  , { s: "Solar", t: "Storage", v: 10.0 }
  , { s: "Storage", t: "Grid", v: 8.0 }
  , { s: "Industrial", t: "Waste Heat", v: 12.0 }
  , { s: "Residential", t: "Waste Heat", v: 5.0 }
  ]

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
  , { s: "Recycling", t: "Raw Material", v: 18.0 } -- end cycle: last layer -> first layer
  , { s: "Scrap", t: "Recycling", v: 4.0 }
  ]

-- | Interior cycle: process with internal feedback loop
interiorCycleData :: Array LinkCSVRow
interiorCycleData =
  [ { s: "Input", t: "Reactor A", v: 40.0 }
  , { s: "Reactor A", t: "Separator", v: 38.0 }
  , { s: "Reactor A", t: "Byproduct", v: 2.0 }
  , { s: "Separator", t: "Product", v: 25.0 }
  , { s: "Separator", t: "Reactor A", v: 13.0 } -- interior cycle: feedback within process
  , { s: "Product", t: "Refinery", v: 20.0 }
  , { s: "Product", t: "Storage", v: 5.0 }
  , { s: "Refinery", t: "Output", v: 18.0 }
  , { s: "Refinery", t: "Byproduct", v: 2.0 }
  ]

-- | Mixed: both end cycles and interior cycles
mixedCycleData :: Array LinkCSVRow
mixedCycleData =
  [ { s: "Ore", t: "Smelter", v: 50.0 }
  , { s: "Smelter", t: "Foundry", v: 40.0 }
  , { s: "Smelter", t: "Slag", v: 10.0 }
  , { s: "Foundry", t: "QC", v: 38.0 }
  , { s: "Foundry", t: "Scrap", v: 2.0 }
  , { s: "QC", t: "Assembly", v: 30.0 }
  , { s: "QC", t: "Foundry", v: 8.0 } -- interior cycle: QC rejects back to Foundry
  , { s: "Assembly", t: "Market", v: 28.0 }
  , { s: "Assembly", t: "Defects", v: 2.0 }
  , { s: "Market", t: "End of Life", v: 20.0 }
  , { s: "Market", t: "Resale", v: 8.0 }
  , { s: "End of Life", t: "Ore", v: 15.0 } -- end cycle: recycling back to raw material
  , { s: "End of Life", t: "Landfill", v: 5.0 }
  , { s: "Scrap", t: "Smelter", v: 2.0 }
  ]

-- =============================================================================
-- Types
-- =============================================================================

type DiagramSpec =
  { title :: String
  , description :: String
  , links :: Array LinkCSVRow
  , containerId :: String
  }

diagrams :: Array DiagramSpec
diagrams =
  [ { title: "Acyclic"
    , description: "Pure DAG \x2014 no cycles. Standard Sankey behavior."
    , links: acyclicData
    , containerId: "ribbon-acyclic"
    }
  , { title: "End-Cyclic"
    , description: "Back-edges from final layer to first layer \x2014 the diagram repeats."
    , links: endCycleData
    , containerId: "ribbon-end-cyclic"
    }
  , { title: "Interior-Cyclic"
    , description: "Back-edges between non-terminal layers \x2014 local feedback loops."
    , links: interiorCycleData
    , containerId: "ribbon-interior-cyclic"
    }
  , { title: "Mixed-Cyclic"
    , description: "Both end cycles and interior cycles present."
    , links: mixedCycleData
    , containerId: "ribbon-mixed-cyclic"
    }
  ]

-- =============================================================================
-- Component
-- =============================================================================

type State = { rendered :: Boolean }

data Action = Initialize

component :: forall query input output m. MonadEffect m => H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> { rendered: false }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
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
        (diagrams <#> renderDiagramCard)
    , renderFooter
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Ribbon Layouts" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "4 cycle topologies for flow diagrams \x2014 from acyclic to mixed-cyclic" ]
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

renderDiagramCard :: forall m. DiagramSpec -> H.ComponentHTML Action () m
renderDiagramCard spec =
  let
    result = Sankey.computeLayout spec.links 800.0 300.0
    topologyLabel = show result.cycleAnalysis.topology
  in
    HH.div
      [ HP.class_ (H.ClassName "ribbon-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "ribbon-card-header") ]
          [ HH.h3_ [ HH.text spec.title ]
          , HH.span
              [ HP.class_ (H.ClassName ("ribbon-topology-badge topology-" <> topologyLabel)) ]
              [ HH.text topologyLabel ]
          ]
      , HH.p
          [ HP.class_ (H.ClassName "ribbon-card-description") ]
          [ HH.text spec.description ]
      , HH.div
          [ HP.class_ (H.ClassName "ribbon-viewport")
          , HP.id spec.containerId
          ]
          []
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

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect renderAllDiagrams
    H.modify_ _ { rendered = true }

renderAllDiagrams :: Effect Unit
renderAllDiagrams = do
  HATS.renderSankeyWide ("#ribbon-acyclic") acyclicData 800.0 300.0
  HATS.renderSankeyWide ("#ribbon-end-cyclic") endCycleData 800.0 300.0
  HATS.renderSankeyWide ("#ribbon-interior-cyclic") interiorCycleData 800.0 300.0
  HATS.renderSankeyWide ("#ribbon-mixed-cyclic") mixedCycleData 800.0 300.0
