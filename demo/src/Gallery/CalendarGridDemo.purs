-- | Calendar Grid Demo
-- |
-- | Interactive demo showing a 7-column weekly calendar grid.
module Gallery.CalendarGridDemo where

import Prelude

import Data.Array as Array
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import DataViz.Layout.Pattern (calendarGrid)
import DataViz.Layout.Pattern.Types (Rect, viewport)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { rects :: Array Rect
  , offset :: Int
  , numDays :: Int
  }

data Action
  = Initialize
  | SetOffset Int
  | SetDays Int

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
  { rects: []
  , offset: 0  -- Sunday
  , numDays: 31
  }

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgElem :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "svg")

rectElem :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
rectElem props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "rect") props []

textElem :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
textElem = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "text")

dayNames :: Array String
dayNames = ["S", "M", "T", "W", "T", "F", "S"]

startDayNames :: Array String
startDayNames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "pattern-demo") ]
    [ renderHeader
    , renderControls state
    , renderViewport state
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Calendar Grid" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "7-column weekly grid with weekday offset" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.a [ HP.href "#pattern" ] [ HH.text "\x2190 Back to Pattern" ] ]
    ]

renderControls :: forall m. State -> H.ComponentHTML Action () m
renderControls state =
  HH.div
    [ HP.class_ (H.ClassName "masonry-controls") ]
    ( [ HH.span [ HP.class_ (H.ClassName "masonry-label") ] [ HH.text "Starts on:" ] ]
      <> ((0 .. 6) <#> \d ->
           HH.button
             [ HP.classes
                 ( [ H.ClassName "masonry-btn" ]
                   <> if d == state.offset then [ H.ClassName "active" ] else []
                 )
             , HE.onClick \_ -> SetOffset d
             ]
             [ HH.text (case Array.index startDayNames d of
                           Just n -> n
                           Nothing -> "?")
             ]
         )
      <> [ HH.span
             [ HP.class_ (H.ClassName "masonry-label")
             , HP.attr (HH.AttrName "style") "margin-left: 16px"
             ]
             [ HH.text "Days:" ]
         , daysBtn 28 state.numDays
         , daysBtn 30 state.numDays
         , daysBtn 31 state.numDays
         ]
    )

daysBtn :: forall m. Int -> Int -> H.ComponentHTML Action () m
daysBtn n current =
  HH.button
    [ HP.classes
        ( [ H.ClassName "masonry-btn" ]
          <> if n == current then [ H.ClassName "active" ] else []
        )
    , HE.onClick \_ -> SetDays n
    ]
    [ HH.text (show n) ]

renderViewport :: forall m. State -> H.ComponentHTML Action () m
renderViewport state =
  let vb = "0 0 700 500"
  in
    HH.div
      [ HP.class_ (H.ClassName "masonry-viewport") ]
      [ svgElem
          [ HP.attr (HH.AttrName "viewBox") vb
          , HP.attr (HH.AttrName "preserveAspectRatio") "xMidYMin meet"
          , HP.attr (HH.AttrName "width") "100%"
          ]
          (Array.mapWithIndex renderDay state.rects)
      ]

renderDay :: forall w i. Int -> Rect -> HH.HTML w i
renderDay i r =
  HH.elementNS (HH.Namespace svgNS) (HH.ElemName "g") []
    [ rectElem
        [ HP.attr (HH.AttrName "x") (show r.x)
        , HP.attr (HH.AttrName "y") (show r.y)
        , HP.attr (HH.AttrName "width") (show r.width)
        , HP.attr (HH.AttrName "height") (show r.height)
        , HP.attr (HH.AttrName "rx") "4"
        , HP.attr (HH.AttrName "ry") "4"
        , HP.attr (HH.AttrName "fill") "#f5f0e1"
        , HP.attr (HH.AttrName "stroke") "#d4c9b0"
        , HP.attr (HH.AttrName "stroke-width") "1"
        ]
    , textElem
        [ HP.attr (HH.AttrName "x") (show (r.x + 8.0))
        , HP.attr (HH.AttrName "y") (show (r.y + 20.0))
        , HP.attr (HH.AttrName "font-size") "14"
        , HP.attr (HH.AttrName "font-family") "Georgia, serif"
        , HP.attr (HH.AttrName "fill") "#3d2b1f"
        ]
        [ HH.text (show (i + 1)) ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> generateLayout
  SetOffset o -> do
    H.modify_ _ { offset = o }
    generateLayout
  SetDays d -> do
    H.modify_ _ { numDays = d }
    generateLayout

generateLayout :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
generateLayout = do
  state <- H.get
  let
    vp = viewport 700.0 500.0
    gap = 4.0
    rects = calendarGrid state.offset state.numDays gap vp
  H.modify_ _ { rects = rects }
