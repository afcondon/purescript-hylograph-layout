-- | Test.Layout.PackSpec
-- |
-- | Golden tests for the Pack (circle packing) layout algorithm.
-- | Tests hierarchical circle packing with enclosing circles.
module Test.Layout.PackSpec
  ( runPackTests
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, stringify)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import DataViz.Layout.Hierarchy.Pack (PackNode(..), HierarchyData(..), hierarchy, pack, packSiblingsMap, defaultPackConfig)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Simple hierarchical data with values
-- |        root (sum=100)
-- |       /    \
-- |      A(60)  B(40)
-- |     / \      \
-- |   C(30) D(30) E(40)
simpleHierarchy :: HierarchyData String
simpleHierarchy =
  HierarchyData
    { data_: "root"
    , value: Nothing
    , children: Just
        [ HierarchyData
            { data_: "A"
            , value: Nothing
            , children: Just
                [ HierarchyData { data_: "C", value: Just 30.0, children: Nothing }
                , HierarchyData { data_: "D", value: Just 30.0, children: Nothing }
                ]
            }
        , HierarchyData
            { data_: "B"
            , value: Nothing
            , children: Just
                [ HierarchyData { data_: "E", value: Just 40.0, children: Nothing }
                ]
            }
        ]
    }

-- | Deep hierarchy (4 levels)
deepHierarchy :: HierarchyData String
deepHierarchy =
  HierarchyData
    { data_: "root"
    , value: Nothing
    , children: Just
        [ HierarchyData
            { data_: "A"
            , value: Nothing
            , children: Just
                [ HierarchyData
                    { data_: "B"
                    , value: Nothing
                    , children: Just
                        [ HierarchyData { data_: "C", value: Just 25.0, children: Nothing }
                        , HierarchyData { data_: "D", value: Just 25.0, children: Nothing }
                        ]
                    }
                ]
            }
        ]
    }

-- | Wide hierarchy (many siblings at same level)
wideHierarchy :: HierarchyData String
wideHierarchy =
  HierarchyData
    { data_: "root"
    , value: Nothing
    , children: Just
        [ HierarchyData { data_: "A", value: Just 10.0, children: Nothing }
        , HierarchyData { data_: "B", value: Just 20.0, children: Nothing }
        , HierarchyData { data_: "C", value: Just 30.0, children: Nothing }
        , HierarchyData { data_: "D", value: Just 25.0, children: Nothing }
        , HierarchyData { data_: "E", value: Just 15.0, children: Nothing }
        ]
    }

-- | Single node hierarchy
singleNode :: HierarchyData String
singleNode =
  HierarchyData
    { data_: "single"
    , value: Just 100.0
    , children: Nothing
    }

-- | Convert PackNode to JSON
packNodeToJson :: PackNode String -> Json
packNodeToJson (PackNode node) = fromObject $ FO.fromFoldable
  [ "name" /\ fromString node.data_
  , "depth" /\ fromNumber (Int.toNumber node.depth)
  , "height" /\ fromNumber (Int.toNumber node.height)
  , "value" /\ fromNumber (roundTo2 node.value)
  , "x" /\ fromNumber (roundTo2 node.x)
  , "y" /\ fromNumber (roundTo2 node.y)
  , "r" /\ fromNumber (roundTo2 node.r)
  , "children" /\ fromArray (map packNodeToJson node.children)
  ]

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Flatten PackNode tree to array (pre-order traversal)
flattenPackNodes :: PackNode String -> Array (PackNode String)
flattenPackNodes node@(PackNode n) =
  [ node ] <> Array.concatMap flattenPackNodes n.children

-- | Run all Pack layout tests
runPackTests :: Effect Int
runPackTests = do
  log "\n=== Pack Layout Tests ==="

  -- Test 1: Simple hierarchy
  log "\nTest 1: Simple hierarchy"
  let hier1 = hierarchy simpleHierarchy
  let result1 = pack defaultPackConfig hier1
  let nodes1 = flattenPackNodes result1
  log $ "  Nodes: " <> show (Array.length nodes1)
  r1 <- assertGolden "pack-simple.golden.json" (stringify $ packNodeToJson result1)
  logResult "Simple hierarchy" r1

  -- Test 2: Deep hierarchy
  log "\nTest 2: Deep hierarchy"
  let hier2 = hierarchy deepHierarchy
  let result2 = pack defaultPackConfig hier2
  let nodes2 = flattenPackNodes result2
  log $ "  Nodes: " <> show (Array.length nodes2)
  r2 <- assertGolden "pack-deep.golden.json" (stringify $ packNodeToJson result2)
  logResult "Deep hierarchy" r2

  -- Test 3: Wide hierarchy
  log "\nTest 3: Wide hierarchy"
  let hier3 = hierarchy wideHierarchy
  let result3 = pack defaultPackConfig hier3
  let nodes3 = flattenPackNodes result3
  log $ "  Nodes: " <> show (Array.length nodes3)
  r3 <- assertGolden "pack-wide.golden.json" (stringify $ packNodeToJson result3)
  logResult "Wide hierarchy" r3

  -- Test 4: Single node
  log "\nTest 4: Single node"
  let hier4 = hierarchy singleNode
  let result4 = pack defaultPackConfig hier4
  let nodes4 = flattenPackNodes result4
  log $ "  Nodes: " <> show (Array.length nodes4)
  r4 <- assertGolden "pack-single.golden.json" (stringify $ packNodeToJson result4)
  logResult "Single node" r4

  -- Test 5: Siblings with tied radii.
  -- Four equal circles beside a larger one give front pairs whose scores tie
  -- exactly, so the result depends on which pairs are scored and in what order.
  -- The golden values are d3-hierarchy's packSiblings output for these radii.
  log "\nTest 5: Tied radii (sibling packing)"
  let result5 = packSiblingsMap (map (\r -> { x: 0.0, y: 0.0, r }) [ 3.0, 1.0, 1.0, 1.0, 1.0 ])
  log $ "  Circles: " <> show (Array.length result5.circles)
  r5 <- assertGolden "pack-siblings-tied.golden.json" (stringify $ siblingsToJson result5)
  logResult "Tied radii" r5

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4, r5]
  log $ "\nPack tests: " <> show (5 - failures) <> "/5 passed"
  pure failures

-- | Packed sibling circles, in input order, with the enclosing radius
siblingsToJson :: { circles :: Array { x :: Number, y :: Number, r :: Number }, radius :: Number } -> Json
siblingsToJson packed = fromObject $ FO.fromFoldable
  [ "radius" /\ fromNumber (roundTo2 packed.radius)
  , "circles" /\ fromArray
      ( map
          ( \c -> fromObject $ FO.fromFoldable
              [ "x" /\ fromNumber (roundTo2 c.x)
              , "y" /\ fromNumber (roundTo2 c.y)
              , "r" /\ fromNumber (roundTo2 c.r)
              ]
          )
          packed.circles
      )
  ]

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
