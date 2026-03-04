-- | Masonry Layout Demo
-- |
-- | Interactive demo showing randomly-sized colored rectangles in masonry form.
-- | Uses DataViz.Layout.Pattern.masonry for layout computation.
module Gallery.MasonryDemo where

import Prelude

import Data.Array as Array
import Data.Array ((..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import DataViz.Layout.Pattern (masonry)
import DataViz.Layout.Pattern.Types (Rect, viewport)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- =============================================================================
-- Types
-- =============================================================================

type State =
  { rects :: Array Rect
  , columns :: Int
  }

data Action
  = Initialize
  | Regenerate
  | SetColumns Int

-- =============================================================================
-- Component
-- =============================================================================

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
  , columns: 3
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

-- Manuscript palette colors
palette :: Array String
palette =
  [ "#c23b22" -- vermillion
  , "#1e3a5f" -- ultramarine
  , "#c9a227" -- gold-leaf
  , "#2d5a27" -- forest-green
  , "#66023c" -- tyrian-purple
  , "#cc7722" -- ochre
  , "#0d6e6e" -- malachite
  , "#5c4033" -- ink-sepia
  ]

colorAt :: Int -> String
colorAt i = case Array.index palette (i `mod` Array.length palette) of
  Just c -> c
  Nothing -> "#999"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "masonry-demo") ]
    [ renderHeader
    , renderControls state.columns
    , renderViewport state
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "gallery-header") ]
    [ HH.h1_ [ HH.text "Masonry Layout" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Pinterest-style column packing \x2014 shortest column gets the next item" ]
    , HH.p
        [ HP.class_ (H.ClassName "hint") ]
        [ HH.a
            [ HP.href "#" ]
            [ HH.text "\x2190 Back to Gallery" ]
        ]
    ]

renderControls :: forall m. Int -> H.ComponentHTML Action () m
renderControls currentCols =
  HH.div
    [ HP.class_ (H.ClassName "masonry-controls") ]
    ( [ HH.span
          [ HP.class_ (H.ClassName "masonry-label") ]
          [ HH.text "Columns:" ]
      ]
      <> colButtons currentCols
      <> [ HH.button
             [ HP.classes [ H.ClassName "masonry-btn", H.ClassName "masonry-btn-regen" ]
             , HE.onClick \_ -> Regenerate
             ]
             [ HH.text "Regenerate" ]
         ]
    )

colButtons :: forall m. Int -> Array (H.ComponentHTML Action () m)
colButtons currentCols =
  (2 .. 5) <#> \n ->
    HH.button
      [ HP.classes
          ( [ H.ClassName "masonry-btn" ]
            <> if n == currentCols then [ H.ClassName "active" ] else []
          )
      , HE.onClick \_ -> SetColumns n
      ]
      [ HH.text (show n) ]

renderViewport :: forall m. State -> H.ComponentHTML Action () m
renderViewport state =
  let
    bounds = foldl
      (\acc r ->
        { maxX: max acc.maxX (r.x + r.width)
        , maxY: max acc.maxY (r.y + r.height)
        }
      )
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

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize ->
    generateLayout

  Regenerate ->
    generateLayout

  SetColumns n -> do
    H.modify_ _ { columns = n }
    generateLayout

generateLayout :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
generateLayout = do
  state <- H.get
  heights <- liftEffect $ generateHeights 25
  let
    vp = viewport 800.0 600.0
    gap = 8.0
    rects = masonry state.columns gap vp heights
  H.modify_ _ { rects = rects }

-- | Generate n random heights in range [40, 200]
generateHeights :: Int -> Effect (Array Number)
generateHeights n = do
  randoms <- traverse (\_ -> random) (Array.replicate n unit)
  pure $ map (\r -> 40.0 + r * 160.0) randoms
