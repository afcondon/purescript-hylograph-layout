module Test.Layout.WaterfallSpec
  ( runWaterfallTests
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
import DataViz.Layout.Pattern (waterfall)
import DataViz.Layout.Pattern.Types (Rect, Viewport, viewport)
import Test.Golden.Util (GoldenResult(..), assertGolden)

rectToJson :: Rect -> Json
rectToJson r = fromObject $ FO.fromFoldable
  [ "x" /\ fromNumber (roundTo2 r.x)
  , "y" /\ fromNumber (roundTo2 r.y)
  , "width" /\ fromNumber (roundTo2 r.width)
  , "height" /\ fromNumber (roundTo2 r.height)
  ]

rectsToJson :: Array Rect -> Json
rectsToJson = fromArray <<< map rectToJson

roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

testViewport :: Viewport
testViewport = viewport 400.0 400.0

runWaterfallTests :: Effect Int
runWaterfallTests = do
  log "\n=== Waterfall Chart Tests ==="

  -- Test 1: All positive deltas
  log "\nTest 1: All positive deltas"
  let result1 = waterfall 40.0 8.0 testViewport
        [100.0, 50.0, 80.0, 30.0]
  log $ "  Rects: " <> show (Array.length result1)
  r1 <- assertGolden "waterfall-positive.golden.json" (stringify $ rectsToJson result1)
  logResult "Positive deltas" r1

  -- Test 2: Mixed positive and negative
  log "\nTest 2: Mixed deltas"
  let result2 = waterfall 40.0 8.0 testViewport
        [100.0, 50.0, -30.0, 80.0, -20.0]
  log $ "  Rects: " <> show (Array.length result2)
  r2 <- assertGolden "waterfall-mixed.golden.json" (stringify $ rectsToJson result2)
  logResult "Mixed deltas" r2

  -- Test 3: All negative deltas
  log "\nTest 3: All negative deltas"
  let result3 = waterfall 40.0 8.0 testViewport
        [-50.0, -30.0, -20.0]
  log $ "  Rects: " <> show (Array.length result3)
  r3 <- assertGolden "waterfall-negative.golden.json" (stringify $ rectsToJson result3)
  logResult "Negative deltas" r3

  -- Test 4: Single delta
  log "\nTest 4: Single delta"
  let result4 = waterfall 40.0 8.0 testViewport [100.0]
  log $ "  Rects: " <> show (Array.length result4)
  r4 <- assertGolden "waterfall-single.golden.json" (stringify $ rectsToJson result4)
  logResult "Single delta" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nWaterfall tests: " <> show (4 - failures) <> "/4 passed"
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

foreign import parseFloat :: String -> Number
