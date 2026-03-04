module Test.Layout.CalendarGridSpec
  ( runCalendarGridTests
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
import DataViz.Layout.Pattern (calendarGrid)
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
testViewport = viewport 700.0 500.0

runCalendarGridTests :: Effect Int
runCalendarGridTests = do
  log "\n=== Calendar Grid Tests ==="

  -- Test 1: March 2026 starts on Sunday (offset 0), 31 days
  log "\nTest 1: March 2026 (Sunday start, 31 days)"
  let result1 = calendarGrid 0 31 4.0 testViewport
  log $ "  Rects: " <> show (Array.length result1)
  r1 <- assertGolden "calendar-march.golden.json" (stringify $ rectsToJson result1)
  logResult "March 2026" r1

  -- Test 2: February starting on Wednesday (offset 3), 28 days
  log "\nTest 2: February (Wednesday start, 28 days)"
  let result2 = calendarGrid 3 28 4.0 testViewport
  log $ "  Rects: " <> show (Array.length result2)
  r2 <- assertGolden "calendar-february.golden.json" (stringify $ rectsToJson result2)
  logResult "February" r2

  -- Test 3: Month starting on Saturday (offset 6), 30 days
  log "\nTest 3: Saturday start, 30 days"
  let result3 = calendarGrid 6 30 4.0 testViewport
  log $ "  Rects: " <> show (Array.length result3)
  r3 <- assertGolden "calendar-saturday.golden.json" (stringify $ rectsToJson result3)
  logResult "Saturday start" r3

  -- Test 4: Short month, no gap
  log "\nTest 4: No gap, 7 days"
  let result4 = calendarGrid 0 7 0.0 testViewport
  log $ "  Rects: " <> show (Array.length result4)
  r4 <- assertGolden "calendar-week.golden.json" (stringify $ rectsToJson result4)
  logResult "One week" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nCalendar Grid tests: " <> show (4 - failures) <> "/4 passed"
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
