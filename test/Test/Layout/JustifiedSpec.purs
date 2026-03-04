module Test.Layout.JustifiedSpec
  ( runJustifiedTests
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
import DataViz.Layout.Pattern (justified)
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
testViewport = viewport 800.0 600.0

runJustifiedTests :: Effect Int
runJustifiedTests = do
  log "\n=== Justified Layout Tests ==="

  -- Test 1: Mixed aspect ratios
  log "\nTest 1: Mixed aspect ratios"
  let result1 = justified 200.0 8.0 testViewport
        [1.5, 0.75, 1.33, 2.0, 0.8, 1.2, 0.9, 1.6]
  log $ "  Rects: " <> show (Array.length result1)
  r1 <- assertGolden "justified-mixed.golden.json" (stringify $ rectsToJson result1)
  logResult "Mixed aspects" r1

  -- Test 2: All landscape (wide items)
  log "\nTest 2: All landscape"
  let result2 = justified 150.0 8.0 testViewport
        [1.5, 1.8, 2.0, 1.3, 1.6]
  log $ "  Rects: " <> show (Array.length result2)
  r2 <- assertGolden "justified-landscape.golden.json" (stringify $ rectsToJson result2)
  logResult "Landscape" r2

  -- Test 3: All portrait (tall items)
  log "\nTest 3: All portrait"
  let result3 = justified 200.0 8.0 testViewport
        [0.5, 0.6, 0.4, 0.7, 0.5, 0.6]
  log $ "  Rects: " <> show (Array.length result3)
  r3 <- assertGolden "justified-portrait.golden.json" (stringify $ rectsToJson result3)
  logResult "Portrait" r3

  -- Test 4: Single item
  log "\nTest 4: Single item"
  let result4 = justified 200.0 8.0 testViewport [1.5]
  log $ "  Rects: " <> show (Array.length result4)
  r4 <- assertGolden "justified-single.golden.json" (stringify $ rectsToJson result4)
  logResult "Single item" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nJustified tests: " <> show (4 - failures) <> "/4 passed"
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
