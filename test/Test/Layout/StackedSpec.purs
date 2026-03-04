module Test.Layout.StackedSpec
  ( runStackedTests
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
import DataViz.Layout.Pattern (stacked, stackedHorizontal, stackedDiverging)
import DataViz.Layout.Pattern.Types (Rect, Viewport, viewport)
import Test.Golden.Util (GoldenResult(..), assertGolden)

rectToJson :: Rect -> Json
rectToJson r = fromObject $ FO.fromFoldable
  [ "x" /\ fromNumber (roundTo2 r.x)
  , "y" /\ fromNumber (roundTo2 r.y)
  , "width" /\ fromNumber (roundTo2 r.width)
  , "height" /\ fromNumber (roundTo2 r.height)
  ]

groupsToJson :: Array (Array Rect) -> Json
groupsToJson = fromArray <<< map (fromArray <<< map rectToJson)

roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

testViewport :: Viewport
testViewport = viewport 400.0 400.0

runStackedTests :: Effect Int
runStackedTests = do
  log "\n=== Stacked Bar Tests ==="

  -- Test 1: Vertical stacked
  log "\nTest 1: Vertical stacked bars"
  let result1 = stacked 8.0 testViewport
        [ [30.0, 20.0, 10.0]
        , [25.0, 35.0, 5.0]
        , [15.0, 15.0, 30.0]
        ]
  log $ "  Groups: " <> show (Array.length result1)
  r1 <- assertGolden "stacked-vertical.golden.json" (stringify $ groupsToJson result1)
  logResult "Vertical stacked" r1

  -- Test 2: Horizontal stacked
  log "\nTest 2: Horizontal stacked bars"
  let result2 = stackedHorizontal 8.0 testViewport
        [ [30.0, 20.0]
        , [40.0, 10.0]
        ]
  log $ "  Groups: " <> show (Array.length result2)
  r2 <- assertGolden "stacked-horizontal.golden.json" (stringify $ groupsToJson result2)
  logResult "Horizontal stacked" r2

  -- Test 3: Diverging stacked
  log "\nTest 3: Diverging stacked bars"
  let result3 = stackedDiverging 8.0 testViewport
        [ [20.0, -10.0, 15.0]
        , [-5.0, 30.0, -15.0]
        ]
  log $ "  Groups: " <> show (Array.length result3)
  r3 <- assertGolden "stacked-diverging.golden.json" (stringify $ groupsToJson result3)
  logResult "Diverging stacked" r3

  -- Test 4: Single bar
  log "\nTest 4: Single bar with segments"
  let result4 = stacked 0.0 testViewport
        [ [40.0, 30.0, 20.0, 10.0] ]
  log $ "  Groups: " <> show (Array.length result4)
  r4 <- assertGolden "stacked-single.golden.json" (stringify $ groupsToJson result4)
  logResult "Single bar" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nStacked tests: " <> show (4 - failures) <> "/4 passed"
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
