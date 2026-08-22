-- | Test.Layout.StateMachineSpec
-- |
-- | Golden tests for the State Machine layout algorithm.
-- | Tests circular and grid layouts for state machine diagrams.
module Test.Layout.StateMachineSpec
  ( runStateMachineTests
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, stringify)
import Data.Array as Array
import Data.Graph.Algorithms (mkSimpleGraph)
import Data.Graph.InducedTree (Induced, induce)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import DataViz.Layout.StateMachine (StateMachine, StateMachineLayout, LayoutState, LayoutTransition, layout, layoutWithConfig, defaultConfig, circularLayout, gridLayout, treeStrategy)
import Test.Golden.Util (GoldenResult(..), assertGolden)

-- | Simple linear state machine: s0 -> s1 -> s2
linearMachine :: StateMachine Unit Unit
linearMachine =
  { states:
      [ { id: "s0", label: "Idle", isInitial: true, isFinal: false, extra: unit }
      , { id: "s1", label: "Running", isInitial: false, isFinal: false, extra: unit }
      , { id: "s2", label: "Done", isInitial: false, isFinal: true, extra: unit }
      ]
  , transitions:
      [ { from: "s0", to: "s1", label: "start", extra: unit }
      , { from: "s1", to: "s2", label: "finish", extra: unit }
      ]
  }

-- | State machine with self-loop
selfLoopMachine :: StateMachine Unit Unit
selfLoopMachine =
  { states:
      [ { id: "s0", label: "Start", isInitial: true, isFinal: false, extra: unit }
      , { id: "s1", label: "Process", isInitial: false, isFinal: false, extra: unit }
      , { id: "s2", label: "End", isInitial: false, isFinal: true, extra: unit }
      ]
  , transitions:
      [ { from: "s0", to: "s1", label: "begin", extra: unit }
      , { from: "s1", to: "s1", label: "tick", extra: unit }  -- Self-loop
      , { from: "s1", to: "s2", label: "done", extra: unit }
      ]
  }

-- | Branching state machine with multiple paths
branchingMachine :: StateMachine Unit Unit
branchingMachine =
  { states:
      [ { id: "s0", label: "Init", isInitial: true, isFinal: false, extra: unit }
      , { id: "s1", label: "Branch A", isInitial: false, isFinal: false, extra: unit }
      , { id: "s2", label: "Branch B", isInitial: false, isFinal: false, extra: unit }
      , { id: "s3", label: "Merge", isInitial: false, isFinal: true, extra: unit }
      ]
  , transitions:
      [ { from: "s0", to: "s1", label: "path A", extra: unit }
      , { from: "s0", to: "s2", label: "path B", extra: unit }
      , { from: "s1", to: "s3", label: "join A", extra: unit }
      , { from: "s2", to: "s3", label: "join B", extra: unit }
      ]
  }

-- | A machine whose initial state cannot reach everything, and whose deepest
-- | state has a way back. Exercises the two things a ring layout hides: the
-- | stranded state (parked in a band below the tree) and the return edge.
strandedMachine :: StateMachine Unit Unit
strandedMachine =
  { states:
      [ { id: "s0", label: "Home", isInitial: true, isFinal: false, extra: unit }
      , { id: "s1", label: "Menu", isInitial: false, isFinal: false, extra: unit }
      , { id: "s2", label: "Detail", isInitial: false, isFinal: false, extra: unit }
      , { id: "s3", label: "Orphan", isInitial: false, isFinal: false, extra: unit }
      ]
  , transitions:
      [ { from: "s0", to: "s1", label: "open", extra: unit }
      , { from: "s1", to: "s2", label: "select", extra: unit }
      , { from: "s2", to: "s0", label: "home", extra: unit }
      , { from: "s3", to: "s0", label: "escape", extra: unit }
      ]
  }

-- | The tree the machine induces from its initial state.
inducedFrom :: forall se te. String -> StateMachine se te -> Induced String
inducedFrom root machine =
  induce root $
    mkSimpleGraph
      (map _.id machine.states)
      (map (\t -> Tuple t.from t.to) machine.transitions)

-- | Convert state position to JSON
stateToJson :: forall e. LayoutState e -> Json
stateToJson { state, position } = fromObject $ FO.fromFoldable
  [ "id" /\ fromString state.id
  , "label" /\ fromString state.label
  , "isInitial" /\ fromBoolean state.isInitial
  , "isFinal" /\ fromBoolean state.isFinal
  , "cx" /\ fromNumber (roundTo2 position.cx)
  , "cy" /\ fromNumber (roundTo2 position.cy)
  , "rx" /\ fromNumber (roundTo2 position.rx)
  , "ry" /\ fromNumber (roundTo2 position.ry)
  ]

-- | Convert transition path to JSON
transitionToJson :: forall e. LayoutTransition e -> Json
transitionToJson { transition, path } = fromObject $ FO.fromFoldable
  [ "from" /\ fromString transition.from
  , "to" /\ fromString transition.to
  , "label" /\ fromString transition.label
  , "startX" /\ fromNumber (roundTo2 path.startX)
  , "startY" /\ fromNumber (roundTo2 path.startY)
  , "controlX" /\ fromNumber (roundTo2 path.controlX)
  , "controlY" /\ fromNumber (roundTo2 path.controlY)
  , "endX" /\ fromNumber (roundTo2 path.endX)
  , "endY" /\ fromNumber (roundTo2 path.endY)
  , "isSelfLoop" /\ fromBoolean path.isSelfLoop
  ]

-- | Convert full layout to JSON
layoutToJson :: forall se te. StateMachineLayout se te -> Json
layoutToJson result = fromObject $ FO.fromFoldable
  [ "states" /\ fromArray (map stateToJson sortedStates)
  , "transitions" /\ fromArray (map transitionToJson sortedTransitions)
  , "width" /\ fromNumber (roundTo2 result.width)
  , "height" /\ fromNumber (roundTo2 result.height)
  ]
  where
  -- Sort for deterministic output
  sortedStates = Array.sortBy (\a b -> compare a.state.id b.state.id) result.states
  sortedTransitions = Array.sortBy (\a b -> compare a.transition.from b.transition.from <> compare a.transition.to b.transition.to) result.transitions

-- | Round to 2 decimal places
roundTo2 :: Number -> Number
roundTo2 n =
  let str = toStringWith (fixed 2) n
  in parseFloat str

-- | Run all State Machine layout tests
runStateMachineTests :: Effect Int
runStateMachineTests = do
  log "\n=== State Machine Layout Tests ==="

  -- Test 1: Linear machine with circular layout (default)
  log "\nTest 1: Linear machine (circular layout)"
  let result1 = layout linearMachine
  log $ "  States: " <> show (Array.length result1.states)
  log $ "  Transitions: " <> show (Array.length result1.transitions)
  r1 <- assertGolden "statemachine-linear-circular.golden.json" (stringify $ layoutToJson result1)
  logResult "Linear circular" r1

  -- Test 2: Self-loop machine with circular layout
  log "\nTest 2: Self-loop machine (circular layout)"
  let result2 = layout selfLoopMachine
  log $ "  States: " <> show (Array.length result2.states)
  log $ "  Transitions: " <> show (Array.length result2.transitions)
  r2 <- assertGolden "statemachine-selfloop-circular.golden.json" (stringify $ layoutToJson result2)
  logResult "Self-loop circular" r2

  -- Test 3: Branching machine with circular layout
  log "\nTest 3: Branching machine (circular layout)"
  let result3 = layout branchingMachine
  log $ "  States: " <> show (Array.length result3.states)
  log $ "  Transitions: " <> show (Array.length result3.transitions)
  r3 <- assertGolden "statemachine-branching-circular.golden.json" (stringify $ layoutToJson result3)
  logResult "Branching circular" r3

  -- Test 4: Branching machine with grid layout
  log "\nTest 4: Branching machine (grid layout)"
  let result4 = layoutWithConfig defaultConfig gridLayout branchingMachine
  log $ "  States: " <> show (Array.length result4.states)
  log $ "  Transitions: " <> show (Array.length result4.transitions)
  r4 <- assertGolden "statemachine-branching-grid.golden.json" (stringify $ layoutToJson result4)
  logResult "Branching grid" r4

  -- Test 5: Branching machine with tree layout
  log "\nTest 5: Branching machine (tree layout)"
  let result5 = layoutWithConfig defaultConfig (treeStrategy (inducedFrom "s0" branchingMachine)) branchingMachine
  log $ "  States: " <> show (Array.length result5.states)
  r5 <- assertGolden "statemachine-branching-tree.golden.json" (stringify $ layoutToJson result5)
  logResult "Branching tree" r5

  -- Test 6: Tree layout parks a state the initial state cannot reach
  log "\nTest 6: Stranded machine (tree layout)"
  let stranded = inducedFrom "s0" strandedMachine
  log $ "  Unreachable: " <> show stranded.unreachable
  let result6 = layoutWithConfig defaultConfig (treeStrategy stranded) strandedMachine
  r6 <- assertGolden "statemachine-stranded-tree.golden.json" (stringify $ layoutToJson result6)
  logResult "Stranded tree" r6

  -- Count failures
  let failures = countFailures [r1, r2, r3, r4, r5, r6]
  log $ "\nState Machine tests: " <> show (6 - failures) <> "/6 passed"
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

-- FFI import for parseFloat
foreign import parseFloat :: String -> Number
