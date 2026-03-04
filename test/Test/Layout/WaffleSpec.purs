module Test.Layout.WaffleSpec
  ( runWaffleTests
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, stringify)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import DataViz.Layout.Pattern (waffle)
import DataViz.Layout.Pattern.Types (Viewport, WaffleCell, viewport)
import Test.Golden.Util (GoldenResult(..), assertGolden)

waffleCellToJson :: WaffleCell -> Json
waffleCellToJson wc = fromObject $ FO.fromFoldable
  [ "x" /\ fromNumber (roundTo2 wc.rect.x)
  , "y" /\ fromNumber (roundTo2 wc.rect.y)
  , "width" /\ fromNumber (roundTo2 wc.rect.width)
  , "height" /\ fromNumber (roundTo2 wc.rect.height)
  , "category" /\ fromNumber (toNumber wc.category)
  ]

waffleCellsToJson :: Array WaffleCell -> Json
waffleCellsToJson = fromArray <<< map waffleCellToJson

roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

testViewport :: Viewport
testViewport = viewport 400.0 400.0

runWaffleTests :: Effect Int
runWaffleTests = do
  log "\n=== Waffle Chart Tests ==="

  -- Test 1: Simple 5x5 with 2 categories
  log "\nTest 1: 5x5 grid, 2 categories"
  let result1 = waffle 5 5 testViewport [15, 10]
  log $ "  Cells: " <> show (Array.length result1)
  r1 <- assertGolden "waffle-5x5.golden.json" (stringify $ waffleCellsToJson result1)
  logResult "5x5 grid" r1

  -- Test 2: 10x10 with 4 categories
  log "\nTest 2: 10x10 grid, 4 categories"
  let result2 = waffle 10 10 testViewport [30, 25, 20, 25]
  log $ "  Cells: " <> show (Array.length result2)
  r2 <- assertGolden "waffle-10x10.golden.json" (stringify $ waffleCellsToJson result2)
  logResult "10x10 grid" r2

  -- Test 3: Underfilled (fewer cells than grid)
  log "\nTest 3: Underfilled grid"
  let result3 = waffle 4 4 testViewport [5, 3]
  log $ "  Cells: " <> show (Array.length result3)
  r3 <- assertGolden "waffle-underfill.golden.json" (stringify $ waffleCellsToJson result3)
  logResult "Underfilled" r3

  -- Test 4: Single category
  log "\nTest 4: Single category"
  let result4 = waffle 3 3 testViewport [9]
  log $ "  Cells: " <> show (Array.length result4)
  r4 <- assertGolden "waffle-single-cat.golden.json" (stringify $ waffleCellsToJson result4)
  logResult "Single category" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nWaffle tests: " <> show (4 - failures) <> "/4 passed"
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
