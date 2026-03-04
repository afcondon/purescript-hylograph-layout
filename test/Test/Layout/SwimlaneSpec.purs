module Test.Layout.SwimlaneSpec
  ( runSwimlaneTests
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
import DataViz.Layout.Pattern (swimlane)
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
testViewport = viewport 800.0 300.0

runSwimlaneTests :: Effect Int
runSwimlaneTests = do
  log "\n=== Swimlane Tests ==="

  -- Test 1: Basic 3 lanes
  log "\nTest 1: Basic 3 lanes"
  let result1 = swimlane 3 4.0 testViewport
        [ { lane: 0, start: 0.0, end: 0.4 }
        , { lane: 1, start: 0.2, end: 0.8 }
        , { lane: 2, start: 0.5, end: 1.0 }
        ]
  log $ "  Rects: " <> show (Array.length result1)
  r1 <- assertGolden "swimlane-basic.golden.json" (stringify $ rectsToJson result1)
  logResult "Basic 3 lanes" r1

  -- Test 2: Overlapping items in same lane
  log "\nTest 2: Overlapping in same lane"
  let result2 = swimlane 2 4.0 testViewport
        [ { lane: 0, start: 0.0, end: 0.5 }
        , { lane: 0, start: 0.3, end: 0.8 }
        , { lane: 1, start: 0.1, end: 0.9 }
        ]
  log $ "  Rects: " <> show (Array.length result2)
  r2 <- assertGolden "swimlane-overlap.golden.json" (stringify $ rectsToJson result2)
  logResult "Overlapping" r2

  -- Test 3: Single lane
  log "\nTest 3: Single lane"
  let result3 = swimlane 1 0.0 testViewport
        [ { lane: 0, start: 0.0, end: 0.3 }
        , { lane: 0, start: 0.4, end: 0.7 }
        , { lane: 0, start: 0.8, end: 1.0 }
        ]
  log $ "  Rects: " <> show (Array.length result3)
  r3 <- assertGolden "swimlane-single.golden.json" (stringify $ rectsToJson result3)
  logResult "Single lane" r3

  -- Test 4: Many lanes
  log "\nTest 4: 5 lanes"
  let result4 = swimlane 5 4.0 testViewport
        [ { lane: 0, start: 0.0, end: 0.5 }
        , { lane: 1, start: 0.1, end: 0.6 }
        , { lane: 2, start: 0.2, end: 0.7 }
        , { lane: 3, start: 0.3, end: 0.8 }
        , { lane: 4, start: 0.4, end: 0.9 }
        ]
  log $ "  Rects: " <> show (Array.length result4)
  r4 <- assertGolden "swimlane-5lanes.golden.json" (stringify $ rectsToJson result4)
  logResult "5 lanes" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nSwimlane tests: " <> show (4 - failures) <> "/4 passed"
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
