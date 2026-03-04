-- | Waffle Chart Demo
-- |
-- | Interactive demo showing proportional unit grid — alternative to pie charts.
module Gallery.WaffleDemo where

import Prelude

import Data.Array as Array
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import DataViz.Layout.Pattern (waffle)
import DataViz.Layout.Pattern.Types (WaffleCell, viewport)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { cells :: Array WaffleCell
  , gridSize :: Int
  }

data Action
  = Initialize
  | SetGridSize Int

component :: forall query input output m. MonadEffect m => H.Component query input output m
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
  { cells: []
  , gridSize: 10
  }

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgElem :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "svg")

rectElem :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
rectElem props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "rect") props []

categoryColors :: Array String
categoryColors =
  [ "#c23b22" -- vermillion
  , "#1e3a5f" -- ultramarine
  , "#c9a227" -- gold-leaf
  , "#2d5a27" -- forest-green
  , "#66023c" -- tyrian-purple
  ]

colorForCategory :: Int -> String
colorForCategory cat = case Array.index categoryColors (cat `mod` Array.length categoryColors) of
  Just c -> c
  Nothing -> "#999"

-- Sample data: survey results (5 categories totalling 100)
sampleCounts :: Array Int
sampleCounts = [35, 25, 20, 12, 8]

categoryLabels :: Array String
categoryLabels = ["Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "pattern-demo") ]
    [ renderHeader
    , renderControls state.gridSize
    , renderLegend
    , renderViewport state
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Waffle Chart" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Proportional unit grid \x2014 each cell represents one unit" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.a [ HP.href "#pattern" ] [ HH.text "\x2190 Back to Pattern" ] ]
    ]

renderControls :: forall m. Int -> H.ComponentHTML Action () m
renderControls currentSize =
  HH.div
    [ HP.class_ (H.ClassName "masonry-controls") ]
    ( [ HH.span [ HP.class_ (H.ClassName "masonry-label") ] [ HH.text "Grid:" ] ]
      <> ((5 .. 3) <#> \n ->  -- 5x5 through 10x10 (step by selecting specific sizes)
           let size = n * 2  -- 10, 8, 6
           in HH.button
                [ HP.classes
                    ( [ H.ClassName "masonry-btn" ]
                      <> if size == currentSize then [ H.ClassName "active" ] else []
                    )
                , HE.onClick \_ -> SetGridSize size
                ]
                [ HH.text (show size <> "\x00d7" <> show size) ]
         )
    )

renderLegend :: forall m. H.ComponentHTML Action () m
renderLegend =
  HH.div
    [ HP.class_ (H.ClassName "waffle-legend") ]
    (Array.mapWithIndex
      (\i label ->
        HH.span
          [ HP.class_ (H.ClassName "waffle-legend-item") ]
          [ HH.span
              [ HP.class_ (H.ClassName "waffle-legend-swatch")
              , HP.attr (HH.AttrName "style") ("background:" <> colorForCategory i)
              ]
              []
          , HH.text label
          ]
      )
      categoryLabels
    )

renderViewport :: forall m. State -> H.ComponentHTML Action () m
renderViewport state =
  let
    vb = "0 0 400 400"
  in
    HH.div
      [ HP.class_ (H.ClassName "masonry-viewport") ]
      [ svgElem
          [ HP.attr (HH.AttrName "viewBox") vb
          , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMid meet"
          , HP.attr (HH.AttrName "width") "100%"
          ]
          (map renderCell state.cells)
      ]

renderCell :: forall w i. WaffleCell -> HH.HTML w i
renderCell wc =
  rectElem
    [ HP.attr (HH.AttrName "x") (show (wc.rect.x + 1.0))
    , HP.attr (HH.AttrName "y") (show (wc.rect.y + 1.0))
    , HP.attr (HH.AttrName "width") (show (max 0.0 (wc.rect.width - 2.0)))
    , HP.attr (HH.AttrName "height") (show (max 0.0 (wc.rect.height - 2.0)))
    , HP.attr (HH.AttrName "rx") "3"
    , HP.attr (HH.AttrName "ry") "3"
    , HP.attr (HH.AttrName "fill") (colorForCategory wc.category)
    , HP.attr (HH.AttrName "opacity") "0.85"
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> generateLayout
  SetGridSize size -> do
    H.modify_ _ { gridSize = size }
    generateLayout

generateLayout :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
generateLayout = do
  state <- H.get
  let
    vp = viewport 400.0 400.0
    cells = waffle state.gridSize state.gridSize vp sampleCounts
  H.modify_ _ { cells = cells }
