-- | Stacked Bar Demo
-- |
-- | Interactive demo showing vertical, horizontal, and diverging stacked bars.
module Gallery.StackedDemo where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import DataViz.Layout.Pattern (stacked, stackedHorizontal, stackedDiverging)
import DataViz.Layout.Pattern.Types (Rect, viewport)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { groups :: Array (Array Rect)
  , variant :: StackedVariant
  }

data StackedVariant = Vertical | Horizontal | Diverging

data Action
  = Initialize
  | SetVariant StackedVariant

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
  { groups: []
  , variant: Vertical
  }

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgElem :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "svg")

rectElem :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
rectElem props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "rect") props []

segmentColors :: Array String
segmentColors =
  [ "#c23b22", "#1e3a5f", "#c9a227", "#2d5a27", "#66023c" ]

segColorAt :: Int -> String
segColorAt i = case Array.index segmentColors (i `mod` Array.length segmentColors) of
  Just c -> c
  Nothing -> "#999"

sampleData :: Array (Array Number)
sampleData =
  [ [30.0, 20.0, 10.0]
  , [25.0, 35.0, 5.0]
  , [15.0, 15.0, 30.0]
  , [40.0, 10.0, 15.0]
  , [20.0, 25.0, 20.0]
  ]

divergingData :: Array (Array Number)
divergingData =
  [ [20.0, -10.0, 15.0, -5.0]
  , [-15.0, 30.0, -10.0, 8.0]
  , [10.0, -25.0, 20.0, -12.0]
  , [-8.0, 15.0, -20.0, 25.0]
  ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "pattern-demo") ]
    [ renderHeader
    , renderControls state.variant
    , renderViewport state
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Stacked Bar Layout" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Grouped segments stacked vertically, horizontally, or diverging from center" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.a [ HP.href "#pattern" ] [ HH.text "\x2190 Back to Pattern" ] ]
    ]

renderControls :: forall m. StackedVariant -> H.ComponentHTML Action () m
renderControls current =
  HH.div
    [ HP.class_ (H.ClassName "masonry-controls") ]
    [ HH.span [ HP.class_ (H.ClassName "masonry-label") ] [ HH.text "Variant:" ]
    , variantBtn "Vertical" Vertical current
    , variantBtn "Horizontal" Horizontal current
    , variantBtn "Diverging" Diverging current
    ]

variantBtn :: forall m. String -> StackedVariant -> StackedVariant -> H.ComponentHTML Action () m
variantBtn label variant current =
  HH.button
    [ HP.classes
        ( [ H.ClassName "masonry-btn" ]
          <> if sameVariant variant current then [ H.ClassName "active" ] else []
        )
    , HE.onClick \_ -> SetVariant variant
    ]
    [ HH.text label ]

sameVariant :: StackedVariant -> StackedVariant -> Boolean
sameVariant Vertical Vertical = true
sameVariant Horizontal Horizontal = true
sameVariant Diverging Diverging = true
sameVariant _ _ = false

renderViewport :: forall m. State -> H.ComponentHTML Action () m
renderViewport state =
  let
    allRects = Array.concat state.groups
    bounds = foldl
      (\acc r -> { maxX: max acc.maxX (r.x + r.width), maxY: max acc.maxY (r.y + r.height) })
      { maxX: 0.0, maxY: 0.0 }
      allRects
    svgW = max 100.0 bounds.maxX
    svgH = max 100.0 bounds.maxY
    vb = "0 0 " <> show svgW <> " " <> show svgH
  in
    HH.div
      [ HP.class_ (H.ClassName "masonry-viewport") ]
      [ svgElem
          [ HP.attr (HH.AttrName "viewBox") vb
          , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMin meet"
          , HP.attr (HH.AttrName "width") "100%"
          ]
          (Array.concat $ map (Array.mapWithIndex renderRect) state.groups)
      ]

renderRect :: forall w i. Int -> Rect -> HH.HTML w i
renderRect segIdx r =
  rectElem
    [ HP.attr (HH.AttrName "x") (show r.x)
    , HP.attr (HH.AttrName "y") (show r.y)
    , HP.attr (HH.AttrName "width") (show r.width)
    , HP.attr (HH.AttrName "height") (show r.height)
    , HP.attr (HH.AttrName "fill") (segColorAt segIdx)
    , HP.attr (HH.AttrName "opacity") "0.85"
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> generateLayout
  SetVariant v -> do
    H.modify_ _ { variant = v }
    generateLayout

generateLayout :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
generateLayout = do
  state <- H.get
  let
    vp = viewport 800.0 400.0
    gap = 10.0
    groups = case state.variant of
      Vertical -> stacked gap vp sampleData
      Horizontal -> stackedHorizontal gap vp sampleData
      Diverging -> stackedDiverging gap vp divergingData
  H.modify_ _ { groups = groups }
