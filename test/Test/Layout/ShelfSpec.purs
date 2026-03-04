module Test.Layout.ShelfSpec
  ( runShelfTests
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
import DataViz.Layout.Pattern (shelf, shelfUniform)
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
testViewport = viewport 400.0 600.0

runShelfTests :: Effect Int
runShelfTests = do
  log "\n=== Shelf Layout Tests ==="

  -- Test 1: Uniform items that fit in one row
  log "\nTest 1: Items fitting in one row"
  let result1 = shelf 8.0 testViewport
        [ { width: 80.0, height: 60.0 }
        , { width: 100.0, height: 60.0 }
        , { width: 90.0, height: 60.0 }
        ]
  log $ "  Rects: " <> show (Array.length result1)
  r1 <- assertGolden "shelf-one-row.golden.json" (stringify $ rectsToJson result1)
  logResult "One row" r1

  -- Test 2: Items wrapping to multiple rows
  log "\nTest 2: Items wrapping to multiple rows"
  let result2 = shelf 8.0 testViewport
        [ { width: 150.0, height: 60.0 }
        , { width: 150.0, height: 80.0 }
        , { width: 150.0, height: 50.0 }
        , { width: 200.0, height: 70.0 }
        , { width: 100.0, height: 90.0 }
        ]
  log $ "  Rects: " <> show (Array.length result2)
  r2 <- assertGolden "shelf-wrap.golden.json" (stringify $ rectsToJson result2)
  logResult "Wrapping" r2

  -- Test 3: shelfUniform
  log "\nTest 3: Uniform height shelf"
  let result3 = shelfUniform 50.0 8.0 testViewport
        [100.0, 120.0, 80.0, 150.0, 90.0, 60.0]
  log $ "  Rects: " <> show (Array.length result3)
  r3 <- assertGolden "shelf-uniform.golden.json" (stringify $ rectsToJson result3)
  logResult "Uniform height" r3

  -- Test 4: Single item
  log "\nTest 4: Single item"
  let result4 = shelf 8.0 testViewport
        [ { width: 200.0, height: 100.0 } ]
  log $ "  Rects: " <> show (Array.length result4)
  r4 <- assertGolden "shelf-single.golden.json" (stringify $ rectsToJson result4)
  logResult "Single item" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nShelf tests: " <> show (4 - failures) <> "/4 passed"
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
