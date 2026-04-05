-- | DataViz.Layout.Sankey.Types
-- |
-- | Pure PureScript implementation of Sankey diagram layout types.
-- | This replaces the FFI-based approach with first-class PureScript types
-- | that integrate naturally with the phantom type system.
module DataViz.Layout.Sankey.Types
  ( LinkCSVRow
  , NodeID(..)
  , LinkID(..)
  , Link(..)
  , NodeIDMap
  , NodeNameMap
  , SankeyNodeInput
  , SankeyNode
  , SankeyLink
  , initialiseSankeyLink
  , SankeyLayoutResult
  , SankeyConfig
  , SankeyStep
  , Alignment(..)
  , LinkColorMode(..)
  , CycleTopology(..)
  , CycleAnalysis
  , RibbonLayout(..)
  , classifyLayout
  , FlowContext
  , NodeValueStrategy(..)
  , sankeyNodeValue
  , constantNodeValue
  , defaultSankeyConfig
  , SankeyGraphModel
  , initialSankeyGraphModel
  , initialiseSankeyNode
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Graph.Weighted (WeightedDigraph)
import Data.Graph.Weighted as WG
import Data.Map (Map, empty, lookup) as Map
import Data.Maybe (Maybe)
import Data.Set as Set

-- | Input format for links from CSV (user-provided flow data with named nodes)
type LinkCSVRow = { s :: String, t :: String, v :: Number }
newtype NodeID = NodeID Int

derive instance eqNodeID :: Eq NodeID
derive instance ordNodeID :: Ord NodeID
instance showNodeID :: Show NodeID where
  show (NodeID i) = "NodeID " <> show i

-- Newtypes to ensure no possible confusion between link and node ids
newtype LinkID = LinkID Int

derive instance eqLinkID :: Eq LinkID
derive instance ordLinkID :: Ord LinkID
instance showLinkID :: Show LinkID where
  show (LinkID i) = "LinkID" <> show i

newtype Link = Link { source :: NodeID, target :: NodeID, value :: Number, id :: LinkID }

instance showLink :: Show Link where
  show (Link l) = "Link: " <> show l.source <> ", " <> show l.target <> ", " <> show l.value <> ", " <> show l.id

type NodeIDMap = Map.Map String NodeID
type NodeNameMap = Map.Map NodeID String

-- | Input format for nodes (minimal user-provided data) - for backward compatibility
type SankeyNodeInput =
  { name :: String
  }

-- | Internal node representation after layout computation
-- | This has all computed properties needed for rendering
type SankeyNode =
  { name :: String
  , x0 :: Number -- Left edge x-coordinate
  , y0 :: Number -- Top edge y-coordinate
  , x1 :: Number -- Right edge x-coordinate
  , y1 :: Number -- Bottom edge y-coordinate
  , value :: Number -- Total flow through node
  , depth :: Int -- Left-to-right distance (for left/justify alignment)
  , nodeHeight :: Int -- Right-to-left distance (for right/center alignment)
  , layer :: Int -- Computed horizontal layer after alignment
  , index :: NodeID -- Original index in input array
  , color :: String -- Computed color
  , sourceLinks :: Set.Set NodeID -- Indices of outgoing links
  , targetLinks :: Set.Set NodeID -- Indices of incoming links
  }

-- | Internal link representation after layout computation
type SankeyLink =
  { sourceIndex :: NodeID -- Index of source node
  , targetIndex :: NodeID -- Index of target node
  , value :: Number -- Flow quantity
  , width :: Number -- Visual width (proportional to value)
  , y0 :: Number -- Y-coordinate at source node
  , y1 :: Number -- Y-coordinate at target node
  , color :: String -- Computed color
  , index :: LinkID -- Index assigned when loading
  }

initialiseSankeyLink :: { source :: NodeID, target :: NodeID, value :: Number, id :: LinkID } -> SankeyLink
initialiseSankeyLink l =
  { sourceIndex: l.source
  , targetIndex: l.target
  , value: l.value
  , width: 0.0
  , y0: 0.0
  , y1: 0.0
  , color: ""
  , index: l.id
  }

-- | Result of Sankey layout computation
type SankeyLayoutResult =
  { nodes :: Array SankeyNode
  , links :: Array SankeyLink
  , cycleAnalysis :: CycleAnalysis
  }

-- | Configuration for Sankey layout algorithm
type SankeyConfig =
  { alignment :: Alignment
  , linkColorMode :: LinkColorMode
  , nodeWidth :: Number -- Width of node rectangles
  , nodePadding :: Number -- Vertical spacing between nodes
  , iterations :: Int -- Number of relaxation iterations (default: 6)
  , extent :: { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
  , nodeValueStrategy :: NodeValueStrategy -- How to compute node values from flows
  }

-- | Node alignment strategy
data Alignment
  = Justify -- Spread nodes to fill width
  | Left -- Align nodes to left
  | Right -- Align nodes to right
  | Center -- Center nodes in their layer

derive instance eqAlignment :: Eq Alignment
derive instance ordAlignment :: Ord Alignment

instance showAlignment :: Show Alignment where
  show Justify = "justify"
  show Left = "left"
  show Right = "right"
  show Center = "center"

-- | Link coloring strategy
data LinkColorMode
  = SourceColor -- Color by source node
  | TargetColor -- Color by target node
  | SourceTargetGradient -- Gradient from source to target
  | StaticColor String -- Single color for all links

derive instance eqLinkColorMode :: Eq LinkColorMode
derive instance ordLinkColorMode :: Ord LinkColorMode

instance showLinkColorMode :: Show LinkColorMode where
  show SourceColor = "source"
  show TargetColor = "target"
  show SourceTargetGradient = "source-target"
  show (StaticColor c) = "static(" <> c <> ")"

-- | Classification of cycle topology in a flow graph.
-- | Determined after layer assignment by examining back-edge endpoints.
data CycleTopology
  = Acyclic -- No cycles: pure DAG
  | EndCyclic -- Back-edges only from final layer to first layer
  | InteriorCyclic -- Back-edges between non-terminal layers
  | MixedCyclic -- Both end-cyclic and interior-cyclic back-edges

derive instance eqCycleTopology :: Eq CycleTopology
derive instance ordCycleTopology :: Ord CycleTopology

instance showCycleTopology :: Show CycleTopology where
  show Acyclic = "Acyclic"
  show EndCyclic = "EndCyclic"
  show InteriorCyclic = "InteriorCyclic"
  show MixedCyclic = "MixedCyclic"

-- | Analysis of back-edges in a flow graph, computed after layout.
type CycleAnalysis =
  { topology :: CycleTopology
  , endCycles :: Array SankeyLink -- Back-edges from max layer to layer 0
  , interiorCycles :: Array SankeyLink -- All other back-edges
  }

-- | Classified layout result. Each constructor carries exactly the data
-- | its rendering strategy needs. Pattern match to select the right renderer.
data RibbonLayout
  = AcyclicLayout
      { nodes :: Array SankeyNode
      , links :: Array SankeyLink
      }
  | EndCyclicLayout
      { nodes :: Array SankeyNode
      , links :: Array SankeyLink
      , endCycles :: Array SankeyLink
      }
  | InteriorCyclicLayout
      { nodes :: Array SankeyNode
      , links :: Array SankeyLink
      , interiorCycles :: Array SankeyLink
      }
  | MixedCyclicLayout
      { nodes :: Array SankeyNode
      , links :: Array SankeyLink
      , endCycles :: Array SankeyLink
      , interiorCycles :: Array SankeyLink
      }

-- | Classify a layout result into the appropriate rendering variant.
classifyLayout :: SankeyLayoutResult -> RibbonLayout
classifyLayout result = case result.cycleAnalysis.topology of
  Acyclic -> AcyclicLayout
    { nodes: result.nodes, links: result.links }
  EndCyclic -> EndCyclicLayout
    { nodes: result.nodes, links: result.links
    , endCycles: result.cycleAnalysis.endCycles }
  InteriorCyclic -> InteriorCyclicLayout
    { nodes: result.nodes, links: result.links
    , interiorCycles: result.cycleAnalysis.interiorCycles }
  MixedCyclic -> MixedCyclicLayout
    { nodes: result.nodes, links: result.links
    , endCycles: result.cycleAnalysis.endCycles
    , interiorCycles: result.cycleAnalysis.interiorCycles }

-- | The flows incident on a node, separated by direction
type FlowContext =
  { incoming :: Array Number
  , outgoing :: Array Number
  }

-- | Strategy for computing a node's value from its incident flows.
-- | This is the primary parameterization point for generalizing
-- | beyond Sankey diagrams to arbitrary ribbon layouts.
newtype NodeValueStrategy = NodeValueStrategy (FlowContext -> Number)

-- | Sankey: node value = max(sum inflows, sum outflows).
-- | The conservation-of-flow constraint that defines a Sankey diagram.
sankeyNodeValue :: NodeValueStrategy
sankeyNodeValue = NodeValueStrategy \{ incoming, outgoing } ->
  max (foldl (+) 0.0 incoming) (foldl (+) 0.0 outgoing)

-- | Constant node size, ignoring flow values entirely.
-- | Useful for pure topology/routing diagrams.
constantNodeValue :: Number -> NodeValueStrategy
constantNodeValue v = NodeValueStrategy \_ -> v

-- | Default configuration matching D3 defaults
defaultSankeyConfig :: Number -> Number -> SankeyConfig
defaultSankeyConfig width height =
  { alignment: Justify -- Justify spreads nodes: sources left, sinks right
  , linkColorMode: SourceTargetGradient  -- Gradient from source to target node colors
  , nodeWidth: 15.0 -- Match D3 default
  , nodePadding: 10.0 -- Match D3 default
  , iterations: 6 -- Must match D3's default for correct beta ramp-up
  -- Set up extent to match D3 FFI: [[1, 1], [width - 1, height - 5]]
  , extent:
      { x0: 1.0
      , y0: 1.0
      , x1: width - 1.0
      , y1: height - 5.0
      }
  , nodeValueStrategy: sankeyNodeValue
  }

-- | A captured step for debugging/visualization
type SankeyStep =
  { iteration :: Int
  , label :: String
  , nodes :: Array SankeyNode
  , links :: Array SankeyLink
  }

-- | Graph model for State monad - contains all intermediate computation state
-- | SankeyGraphModel is created by folding rows of CSV
type SankeyGraphModel =
  { linkCount :: Int
  , nodeCount :: Int
  , nodeNameToID :: NodeIDMap
  , nodeIDToName :: NodeNameMap
  , nodeOrder :: Array NodeID -- Nodes in encounter order (matches D3's Set insertion order)
  , graph :: WeightedDigraph NodeID Number -- Directed graph for adjacency lookups
  , sankeyNodes :: Array SankeyNode
  , sankeyLinks :: Array SankeyLink -- Forward links (used for layout)
  , backEdgeLinks :: Array SankeyLink -- Back-edge links (excluded from layout)
  , config :: SankeyConfig
  , capturedSteps :: Array SankeyStep -- For debugging: intermediate states
  }

-- Initial empty graph model
initialSankeyGraphModel :: SankeyConfig -> SankeyGraphModel
initialSankeyGraphModel config =
  { linkCount: 0
  , nodeCount: 0
  , nodeNameToID: Map.empty
  , nodeIDToName: Map.empty
  , nodeOrder: [] -- Will be populated in encounter order
  , graph: WG.empty
  , sankeyNodes: []
  , sankeyLinks: []
  , backEdgeLinks: []
  , config
  , capturedSteps: []
  }

initialiseSankeyNode :: SankeyGraphModel -> NodeID -> Maybe SankeyNode
initialiseSankeyNode m id = do
  name <- Map.lookup id m.nodeIDToName
  -- Get adjacency from graph: outgoing targets and incoming sources
  let sourceLinks = Set.fromFoldable $ map _.target $ WG.outgoing id m.graph
  let targetLinks = Set.fromFoldable $ map _.source $ WG.incoming id m.graph
  pure $
    { name
    , x0: 0.0
    , x1: 0.0
    , y0: 0.0
    , y1: 0.0
    , value: 0.0
    , depth: 0
    , nodeHeight: 0
    , layer: 0
    , index: id
    , color: ""
    , sourceLinks
    , targetLinks
    }

