-- | Pattern Page
-- |
-- | Composes all 9 pattern demo components on one scrollable page
-- | using Halogen child component slots.
module Gallery.PatternPage where

import Prelude

import Data.Const (Const)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import Gallery.MasonryDemo as MasonryDemo
import Gallery.ShelfDemo as ShelfDemo
import Gallery.WaffleDemo as WaffleDemo
import Gallery.StackedDemo as StackedDemo
import Gallery.WaterfallDemo as WaterfallDemo
import Gallery.JustifiedDemo as JustifiedDemo
import Gallery.BinPackDemo as BinPackDemo
import Gallery.CalendarGridDemo as CalendarGridDemo
import Gallery.SwimlaneDemo as SwimlaneDemo

-- =============================================================================
-- Types
-- =============================================================================

type Slots =
  ( masonry :: H.Slot (Const Void) Void Unit
  , shelf :: H.Slot (Const Void) Void Unit
  , waffle :: H.Slot (Const Void) Void Unit
  , stacked :: H.Slot (Const Void) Void Unit
  , waterfall :: H.Slot (Const Void) Void Unit
  , justified :: H.Slot (Const Void) Void Unit
  , binpack :: H.Slot (Const Void) Void Unit
  , calendar :: H.Slot (Const Void) Void Unit
  , swimlane :: H.Slot (Const Void) Void Unit
  )

_masonry = Proxy :: Proxy "masonry"
_shelf = Proxy :: Proxy "shelf"
_waffle = Proxy :: Proxy "waffle"
_stacked = Proxy :: Proxy "stacked"
_waterfall = Proxy :: Proxy "waterfall"
_justified = Proxy :: Proxy "justified"
_binpack = Proxy :: Proxy "binpack"
_calendar = Proxy :: Proxy "calendar"
_swimlane = Proxy :: Proxy "swimlane"

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadEffect m => H.Component query input output m
component = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval H.defaultEval
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadEffect m => H.ComponentHTML Void Slots m
render =
  HH.div
    [ HP.class_ (H.ClassName "pattern-page") ]
    [ renderHeader
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _masonry unit MasonryDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _shelf unit ShelfDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _waffle unit WaffleDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _stacked unit StackedDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _waterfall unit WaterfallDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _justified unit JustifiedDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _binpack unit BinPackDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _calendar unit CalendarGridDemo.component unit absurd ]
    , sectionDivider
    , HH.div [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.slot _swimlane unit SwimlaneDemo.component unit absurd ]
    , renderFooter
    ]

renderHeader :: forall m. MonadEffect m => H.ComponentHTML Void Slots m
renderHeader =
  HH.div
    [ HP.class_ (H.ClassName "pattern-page-header") ]
    [ HH.div
        [ HP.class_ (H.ClassName "gallery-header") ]
        [ HH.h1_ [ HH.text "Pattern Layouts" ]
        , HH.p
            [ HP.class_ (H.ClassName "subtitle") ]
            [ HH.text "9 layout patterns for arrangement and composition" ]
        , HH.p
            [ HP.class_ (H.ClassName "gallery-nav") ]
            [ HH.a [ HP.href "#" ] [ HH.text "\x2190 Gallery" ]
            , HH.text " \x00b7 "
            , HH.a [ HP.href "#hierarchy" ] [ HH.text "Hierarchy" ]
            , HH.text " \x00b7 "
            , HH.a [ HP.href "#flow" ] [ HH.text "Flow" ]
            ]
        ]
    ]

sectionDivider :: forall m. MonadEffect m => H.ComponentHTML Void Slots m
sectionDivider =
  HH.div
    [ HP.class_ (H.ClassName "pattern-divider") ]
    [ HH.span_ [ HH.text "\x2766" ] ]

renderFooter :: forall m. MonadEffect m => H.ComponentHTML Void Slots m
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
