-- | Swimlane Demo
-- |
-- | Interactive demo showing horizontal lanes with positioned items — for timelines, Gantt charts.
module Gallery.SwimlaneDemo where

import Prelude

import Data.Array as Array
import Data.Array ((..))
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import DataViz.Layout.Pattern (swimlane)
import DataViz.Layout.Pattern.Types (Rect, viewport)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { rects :: Array Rect
  , numLanes :: Int
  }

data Action
  = Initialize
  | Regenerate
  | SetLanes Int

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
  , numLanes: 4
  }

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
    [ HP.class_ (H.ClassName "pattern-demo") ]
    [ renderHeader
    , renderControls state.numLanes
    , renderViewport state
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Swimlane Layout" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Horizontal lanes with positioned items \x2014 for timelines and Gantt charts" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.a [ HP.href "#pattern" ] [ HH.text "\x2190 Back to Pattern" ] ]
    ]

renderControls :: forall m. Int -> H.ComponentHTML Action () m
renderControls currentLanes =
  HH.div
    [ HP.class_ (H.ClassName "masonry-controls") ]
    ( [ HH.span [ HP.class_ (H.ClassName "masonry-label") ] [ HH.text "Lanes:" ] ]
      <> ((2 .. 6) <#> \n ->
           HH.button
             [ HP.classes
                 ( [ H.ClassName "masonry-btn" ]
                   <> if n == currentLanes then [ H.ClassName "active" ] else []
                 )
             , HE.onClick \_ -> SetLanes n
             ]
             [ HH.text (show n) ]
         )
      <> [ HH.button
             [ HP.classes [ H.ClassName "masonry-btn", H.ClassName "masonry-btn-regen" ]
             , HE.onClick \_ -> Regenerate
             ]
             [ HH.text "Regenerate" ]
         ]
    )

renderViewport :: forall m. State -> H.ComponentHTML Action () m
renderViewport state =
  let
    bounds = foldl
      (\acc r -> { maxX: max acc.maxX (r.x + r.width), maxY: max acc.maxY (r.y + r.height) })
      { maxX: 0.0, maxY: 0.0 }
      state.rects
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
          (Array.mapWithIndex renderRect state.rects)
      ]

renderRect :: forall w i. Int -> Rect -> HH.HTML w i
renderRect i r =
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

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> generateLayout
  Regenerate -> generateLayout
  SetLanes n -> do
    H.modify_ _ { numLanes = n }
    generateLayout

generateLayout :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
generateLayout = do
  state <- H.get
  items <- liftEffect $ generateItems state.numLanes 12
  let
    vp = viewport 800.0 400.0
    gap = 6.0
    rects = swimlane state.numLanes gap vp items
  H.modify_ _ { rects = rects }

generateItems :: Int -> Int -> Effect (Array { lane :: Int, start :: Number, end :: Number })
generateItems numLanes n =
  traverse (\_ -> do
    rl <- random
    rs <- random
    rd <- random
    let
      lane = min (numLanes - 1) (max 0 (floor (rl * toNumber numLanes)))
      start' = rs * 0.7
      duration = 0.1 + rd * 0.3
      end' = min 1.0 (start' + duration)
    pure { lane, start: start', end: end' }
  ) (Array.replicate n unit)
