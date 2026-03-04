module Test.Layout.AdaptersSpec
  ( runAdaptersTests
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
import DataViz.Layout.Pattern.Adapters (class HasAspectRatio, class Sized, class Temporal, class TimeInterval, calendarFrom, justifiedFrom, shelfFrom, swimlaneFrom)
import DataViz.Layout.Pattern.Types (Rect, Viewport, viewport)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- Test data types with typeclass instances

newtype TestDay = TestDay { dow :: Int, idx :: Int, label :: String }

instance temporalTestDay :: Temporal TestDay where
  dayOfWeek (TestDay d) = d.dow
  dayIndex (TestDay d) = d.idx

newtype TestInterval = TestInterval { lane :: Int, start :: Number, end :: Number, name :: String }

instance timeIntervalTestInterval :: TimeInterval TestInterval where
  intervalLane (TestInterval i) = i.lane
  intervalStart (TestInterval i) = i.start
  intervalEnd (TestInterval i) = i.end

newtype TestBox = TestBox { w :: Number, h :: Number, id :: Int }

instance sizedTestBox :: Sized TestBox where
  itemWidth (TestBox b) = b.w
  itemHeight (TestBox b) = b.h

newtype TestImage = TestImage { aspect :: Number, title :: String }

instance hasAspectRatioTestImage :: HasAspectRatio TestImage where
  aspectRatio (TestImage img) = img.aspect

-- JSON helpers

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

runAdaptersTests :: Effect Int
runAdaptersTests = do
  log "\n=== Adapters Tests ==="

  -- Test 1: calendarFrom
  log "\nTest 1: calendarFrom"
  let days = [ TestDay { dow: 0, idx: 0, label: "Sun" }
             , TestDay { dow: 1, idx: 1, label: "Mon" }
             , TestDay { dow: 2, idx: 2, label: "Tue" }
             , TestDay { dow: 3, idx: 3, label: "Wed" }
             , TestDay { dow: 4, idx: 4, label: "Thu" }
             , TestDay { dow: 5, idx: 5, label: "Fri" }
             , TestDay { dow: 6, idx: 6, label: "Sat" }
             ]
  let result1 = calendarFrom 4.0 testViewport days
  log $ "  Results: " <> show (Array.length result1)
  r1 <- assertGolden "adapters-calendar.golden.json" (stringify $ rectsToJson (map _.rect result1))
  logResult "calendarFrom" r1

  -- Test 2: swimlaneFrom
  log "\nTest 2: swimlaneFrom"
  let intervals =
        [ TestInterval { lane: 0, start: 0.0, end: 0.4, name: "Task A" }
        , TestInterval { lane: 1, start: 0.2, end: 0.7, name: "Task B" }
        , TestInterval { lane: 0, start: 0.5, end: 0.9, name: "Task C" }
        ]
  let result2 = swimlaneFrom 2 4.0 testViewport intervals
  log $ "  Results: " <> show (Array.length result2)
  r2 <- assertGolden "adapters-swimlane.golden.json" (stringify $ rectsToJson (map _.rect result2))
  logResult "swimlaneFrom" r2

  -- Test 3: shelfFrom
  log "\nTest 3: shelfFrom"
  let boxes =
        [ TestBox { w: 100.0, h: 60.0, id: 1 }
        , TestBox { w: 150.0, h: 80.0, id: 2 }
        , TestBox { w: 200.0, h: 50.0, id: 3 }
        , TestBox { w: 120.0, h: 70.0, id: 4 }
        ]
  let result3 = shelfFrom 8.0 testViewport boxes
  log $ "  Results: " <> show (Array.length result3)
  r3 <- assertGolden "adapters-shelf.golden.json" (stringify $ rectsToJson (map _.rect result3))
  logResult "shelfFrom" r3

  -- Test 4: justifiedFrom + binPackFrom
  log "\nTest 4: justifiedFrom"
  let images =
        [ TestImage { aspect: 1.5, title: "Landscape" }
        , TestImage { aspect: 0.75, title: "Portrait" }
        , TestImage { aspect: 1.0, title: "Square" }
        , TestImage { aspect: 2.0, title: "Panorama" }
        ]
  let result4 = justifiedFrom 200.0 8.0 testViewport images
  log $ "  Results: " <> show (Array.length result4)
  r4 <- assertGolden "adapters-justified.golden.json" (stringify $ rectsToJson (map _.rect result4))
  logResult "justifiedFrom" r4

  let failures = countFailures [r1, r2, r3, r4]
  log $ "\nAdapters tests: " <> show (4 - failures) <> "/4 passed"
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
