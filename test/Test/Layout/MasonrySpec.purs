-- | Test.Layout.MasonrySpec
-- |
-- | Golden tests for the Masonry layout algorithm.
-- | Tests Pinterest-style column-filling layouts with varying item heights.
module Test.Layout.MasonrySpec
  ( runMasonryTests
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, stringify)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import DataViz.Layout.Pattern (masonry, masonryAuto)
import DataViz.Layout.Pattern.Types (Rect, Viewport, viewport)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Convert Rect to JSON
rectToJson :: Rect -> Json
rectToJson r = fromObject $ FO.fromFoldable
  [ "x" /\ fromNumber (roundTo2 r.x)
  , "y" /\ fromNumber (roundTo2 r.y)
  , "width" /\ fromNumber (roundTo2 r.width)
  , "height" /\ fromNumber (roundTo2 r.height)
  ]

-- | Convert array of Rects to JSON
rectsToJson :: Array Rect -> Json
rectsToJson = fromArray <<< map rectToJson

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Test viewport: 400x600, no padding
testViewport :: Viewport
testViewport = viewport 400.0 600.0

-- | Run all Masonry layout tests
runMasonryTests :: Effect Int
runMasonryTests = do
  log "\n=== Masonry Layout Tests ==="

  -- Test 1: Uniform heights, 3 columns
  log "\nTest 1: Uniform heights (3 columns)"
  let result1 = masonry 3 10.0 testViewport
        [100.0, 100.0, 100.0, 100.0, 100.0, 100.0]
  log $ "  Rects: " <> show (Array.length result1)
  r1 <- assertGolden "masonry-uniform.golden.json" (stringify $ rectsToJson result1)
  logResult "Uniform heights" r1

  -- Test 2: Varying heights, 3 columns
  log "\nTest 2: Varying heights (3 columns)"
  let result2 = masonry 3 10.0 testViewport
        [100.0, 150.0, 80.0, 200.0, 120.0, 60.0, 140.0]
  log $ "  Rects: " <> show (Array.length result2)
  r2 <- assertGolden "masonry-varying.golden.json" (stringify $ rectsToJson result2)
  logResult "Varying heights" r2

  -- Test 3: Single column
  log "\nTest 3: Single column"
  let result3 = masonry 1 10.0 testViewport
        [100.0, 150.0, 80.0]
  log $ "  Rects: " <> show (Array.length result3)
  r3 <- assertGolden "masonry-single-col.golden.json" (stringify $ rectsToJson result3)
  logResult "Single column" r3

  -- Test 4: Auto columns from max width
  log "\nTest 4: Auto columns (maxColWidth=150)"
  let result4 = masonryAuto 150.0 10.0 testViewport
        [100.0, 150.0, 80.0, 200.0, 120.0]
  log $ "  Rects: " <> show (Array.length result4)
  r4 <- assertGolden "masonry-auto.golden.json" (stringify $ rectsToJson result4)
  logResult "Auto columns" r4

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nMasonry tests: " <> show (4 - failures) <> "/4 passed"
  pure failures

logResult :: String -> GoldenResult -> Effect Unit
logResult name GoldenMatch = log $ "  PASS: " <> name
logResult name GoldenCreated = log $ "  CREATED: " <> name <> " (golden file created)"
logResult name (GoldenMismatch _ _) = log $ "  FAIL: " <> name <> " (output differs)"

countFailures :: Array GoldenResult -> Int
countFailures results = go 0 results
  where
  go n arr = case Array.uncons arr of
    Nothing -> n
    Just { head: GoldenMismatch _ _, tail } -> go (n + 1) tail
    Just { head: _, tail } -> go n tail

-- FFI import for parseFloat
foreign import parseFloat :: String -> Number
