module Test.Layout.BinPackSpec
  ( runBinPackTests
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
import DataViz.Layout.Pattern (binPack)
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

runBinPackTests :: Effect Int
runBinPackTests = do
  log "\n=== Bin Pack Tests ==="

  -- Test 1: Uniform rectangles
  log "\nTest 1: Uniform rectangles"
  let result1 = binPack testViewport
        [ { width: 100.0, height: 100.0 }
        , { width: 100.0, height: 100.0 }
        , { width: 100.0, height: 100.0 }
        , { width: 100.0, height: 100.0 }
        ]
  log $ "  Rects: " <> show (Array.length result1)
  r1 <- assertGolden "binpack-uniform.golden.json" (stringify $ rectsToJson result1)
  logResult "Uniform" r1

  -- Test 2: Varying sizes
  log "\nTest 2: Varying sizes"
  let result2 = binPack testViewport
        [ { width: 150.0, height: 80.0 }
        , { width: 80.0, height: 120.0 }
        , { width: 200.0, height: 60.0 }
        , { width: 100.0, height: 100.0 }
        , { width: 60.0, height: 180.0 }
        ]
  log $ "  Rects: " <> show (Array.length result2)
  r2 <- assertGolden "binpack-varying.golden.json" (stringify $ rectsToJson result2)
  logResult "Varying sizes" r2

  -- Test 3: Single large rectangle
  log "\nTest 3: Single rectangle"
  let result3 = binPack testViewport
        [ { width: 300.0, height: 200.0 } ]
  log $ "  Rects: " <> show (Array.length result3)
  r3 <- assertGolden "binpack-single.golden.json" (stringify $ rectsToJson result3)
  logResult "Single" r3

  -- Test 4: Many small rectangles
  log "\nTest 4: Many small rectangles"
  let result4 = binPack testViewport
        [ { width: 50.0, height: 50.0 }
        , { width: 60.0, height: 40.0 }
        , { width: 40.0, height: 70.0 }
        , { width: 80.0, height: 30.0 }
        , { width: 45.0, height: 55.0 }
        , { width: 70.0, height: 45.0 }
        , { width: 55.0, height: 60.0 }
        , { width: 35.0, height: 35.0 }
        ]
  log $ "  Rects: " <> show (Array.length result4)
  r4 <- assertGolden "binpack-many.golden.json" (stringify $ rectsToJson result4)
  logResult "Many small" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nBin Pack tests: " <> show (4 - failures) <> "/4 passed"
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
