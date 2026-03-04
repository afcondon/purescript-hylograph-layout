-- | Waterfall Chart Demo
-- |
-- | Interactive demo showing cumulative bars where each starts where the previous ended.
module Gallery.WaterfallDemo where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import DataViz.Layout.Pattern (waterfall)
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
  , deltas :: Array Number
  }

data Action
  = Initialize
  | Regenerate

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
  , deltas: []
  }

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgElem :: forall r w i. Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElem = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "svg")

rectElem :: forall r w i. Array (HH.IProp r i) -> HH.HTML w i
rectElem props = HH.elementNS (HH.Namespace svgNS) (HH.ElemName "rect") props []

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "pattern-demo") ]
    [ renderHeader
    , renderControls
    , renderViewport state
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Waterfall Chart" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Cumulative bars \x2014 each starts where the previous ended" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.a [ HP.href "#pattern" ] [ HH.text "\x2190 Back to Pattern" ] ]
    ]

renderControls :: forall m. H.ComponentHTML Action () m
renderControls =
  HH.div
    [ HP.class_ (H.ClassName "masonry-controls") ]
    [ HH.button
        [ HP.classes [ H.ClassName "masonry-btn", H.ClassName "masonry-btn-regen" ]
        , HE.onClick \_ -> Regenerate
        ]
        [ HH.text "Regenerate" ]
    ]

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
          (Array.mapWithIndex (renderBar state.deltas) state.rects)
      ]

renderBar :: forall w i. Array Number -> Int -> Rect -> HH.HTML w i
renderBar deltas i r =
  let delta = fromMaybe 0.0 (Array.index deltas i)
  in rectElem
    [ HP.attr (HH.AttrName "x") (show r.x)
    , HP.attr (HH.AttrName "y") (show r.y)
    , HP.attr (HH.AttrName "width") (show r.width)
    , HP.attr (HH.AttrName "height") (show r.height)
    , HP.attr (HH.AttrName "rx") "2"
    , HP.attr (HH.AttrName "ry") "2"
    , HP.attr (HH.AttrName "fill") (if delta >= 0.0 then "#2d5a27" else "#c23b22")
    , HP.attr (HH.AttrName "opacity") "0.85"
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> generateLayout
  Regenerate -> generateLayout

generateLayout :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
generateLayout = do
  deltas <- liftEffect $ generateDeltas 12
  let
    vp = viewport 800.0 400.0
    rects = waterfall 45.0 8.0 vp deltas
  H.modify_ _ { rects = rects, deltas = deltas }

generateDeltas :: Int -> Effect (Array Number)
generateDeltas n =
  traverse (\_ -> do
    r <- random
    pure (r * 200.0 - 80.0)  -- Range [-80, 120], biased positive
  ) (Array.replicate n unit)
