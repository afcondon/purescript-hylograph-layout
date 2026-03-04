-- | Typeclass adapters for layout functions
-- |
-- | Provides typeclasses that convert real-world data types into the raw numbers
-- | that core layout functions expect. Convenience functions extract, layout, then
-- | zip results back with original data.
-- |
-- | No new dependencies — typeclasses use raw `Int`/`Number`. Callers provide
-- | instances using their own date/time library.
module DataViz.Layout.Pattern.Adapters
  ( class Temporal
  , dayOfWeek
  , dayIndex
  , class TimeInterval
  , intervalLane
  , intervalStart
  , intervalEnd
  , class Sized
  , itemWidth
  , itemHeight
  , class HasAspectRatio
  , aspectRatio
  , calendarFrom
  , swimlaneFrom
  , shelfFrom
  , justifiedFrom
  , binPackFrom
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import DataViz.Layout.Pattern (calendarGrid, swimlane, shelf, justified, binPack)
import DataViz.Layout.Pattern.Types (Rect, Viewport)

-- =============================================================================
-- Typeclasses
-- =============================================================================

-- | Types that can be mapped to calendar day positions.
-- | `dayOfWeek` returns 0=Sunday..6=Saturday.
-- | `dayIndex` returns a sequential day number (0-based within the range).
class Temporal a where
  dayOfWeek :: a -> Int
  dayIndex :: a -> Int

-- | Types that represent time intervals in a lane.
-- | `intervalStart` and `intervalEnd` return normalized values 0.0-1.0.
class TimeInterval a where
  intervalLane :: a -> Int
  intervalStart :: a -> Number
  intervalEnd :: a -> Number

-- | Types that have width and height dimensions.
class Sized a where
  itemWidth :: a -> Number
  itemHeight :: a -> Number

-- | Types that have an intrinsic aspect ratio (width/height).
class HasAspectRatio a where
  aspectRatio :: a -> Number

-- =============================================================================
-- Convenience functions: extract → layout → zip
-- =============================================================================

-- | Layout temporal data on a calendar grid.
calendarFrom :: forall a. Temporal a => Number -> Viewport -> Array a -> Array { datum :: a, rect :: Rect }
calendarFrom gap vp items =
  case Array.head items of
    Nothing -> []
    Just first ->
      let
        offset = dayOfWeek first
        numDays = Array.length items
        rects = calendarGrid offset numDays gap vp
      in
        Array.zipWith (\datum rect -> { datum, rect }) items rects

-- | Layout time intervals in swimlanes.
swimlaneFrom :: forall a. TimeInterval a => Int -> Number -> Viewport -> Array a -> Array { datum :: a, rect :: Rect }
swimlaneFrom numLanes gap vp items =
  let
    entries = map (\item -> { lane: intervalLane item, start: intervalStart item, end: intervalEnd item }) items
    rects = swimlane numLanes gap vp entries
  in
    Array.zipWith (\datum rect -> { datum, rect }) items rects

-- | Layout sized items on a shelf.
shelfFrom :: forall a. Sized a => Number -> Viewport -> Array a -> Array { datum :: a, rect :: Rect }
shelfFrom gap vp items =
  let
    sizes = map (\item -> { width: itemWidth item, height: itemHeight item }) items
    rects = shelf gap vp sizes
  in
    Array.zipWith (\datum rect -> { datum, rect }) items rects

-- | Layout items with aspect ratios in a justified gallery.
justifiedFrom :: forall a. HasAspectRatio a => Number -> Number -> Viewport -> Array a -> Array { datum :: a, rect :: Rect }
justifiedFrom targetRowHeight gap vp items =
  let
    aspects = map aspectRatio items
    rects = justified targetRowHeight gap vp aspects
  in
    Array.zipWith (\datum rect -> { datum, rect }) items rects

-- | Bin-pack sized items into a viewport.
binPackFrom :: forall a. Sized a => Viewport -> Array a -> Array { datum :: a, rect :: Rect }
binPackFrom vp items =
  let
    sizes = map (\item -> { width: itemWidth item, height: itemHeight item }) items
    rects = binPack vp sizes
  in
    Array.zipWith (\datum rect -> { datum, rect }) items rects
