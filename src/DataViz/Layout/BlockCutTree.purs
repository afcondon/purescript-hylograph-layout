-- | Block-Cut Tree Layout
-- |
-- | Positions nodes of a graph based on its biconnected component decomposition.
-- | Biconnected components (blocks) are arranged in BFS layers from the largest
-- | block outward; nodes within each block are arranged in a circle around the
-- | block center. Isolated nodes (no edges) are placed along the bottom.
-- |
-- | This layout is useful for visualizing the structural decomposition of code
-- | modules, social networks, or any graph where cluster structure matters.
module DataViz.Layout.BlockCutTree
  ( BlockCutLayout
  , BlockCutConfig
  , NodeLayout
  , BlockLayout
  , defaultConfig
  , layout
  ) where

import Prelude

import Data.Array as Array
import Data.Array (mapWithIndex)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Set as Set
import Data.Tuple (Tuple(..))

import Data.Graph.Algorithms (SimpleGraph)
import Data.Graph.Decomposition (blockCutTree, articulationPoints)

-- =============================================================================
-- Types
-- =============================================================================

-- | Configuration for the layout algorithm
type BlockCutConfig =
  { width :: Number         -- SVG/canvas width
  , height :: Number        -- SVG/canvas height (0 = auto)
  , margin :: Number        -- Margin around edges
  , minHeight :: Number     -- Minimum height
  , layerGap :: Number      -- Vertical gap between BFS layers
  , maxBlockRadius :: Number -- Maximum radius for a block's node circle
  , minBlockRadius :: Number -- Minimum radius for a block's node circle
  , blockRadiusScale :: Number -- Scale factor for block radius from node count
  , isolatedSpacing :: Number  -- Horizontal spacing for isolated nodes
  , isolatedGap :: Number      -- Vertical gap before isolated row
  }

-- | Per-node layout result
type NodeLayout =
  { x :: Number
  , y :: Number
  , block :: Int               -- Block index (-1 for isolated nodes)
  , isArticulationPoint :: Boolean
  , isIsolated :: Boolean
  }

-- | Per-block layout result
type BlockLayout =
  { x :: Number
  , y :: Number
  , radius :: Number
  , nodeCount :: Int
  , isBridge :: Boolean        -- True if this block has exactly 2 nodes (bridge edge)
  , depth :: Int               -- BFS depth from root block
  }

-- | Complete layout result
type BlockCutLayout node =
  { nodes :: Map.Map node NodeLayout
  , blocks :: Map.Map Int BlockLayout
  , width :: Number
  , height :: Number
  }

-- | Sensible defaults
defaultConfig :: BlockCutConfig
defaultConfig =
  { width: 900.0
  , height: 0.0            -- 0 = auto-size from content
  , margin: 40.0
  , minHeight: 200.0
  , layerGap: 40.0         -- gap between BFS layers (added to radii)
  , maxBlockRadius: 120.0
  , minBlockRadius: 20.0
  , blockRadiusScale: 14.0
  , isolatedSpacing: 16.0
  , isolatedGap: 30.0      -- gap before isolated node row
  }

-- =============================================================================
-- Layout computation
-- =============================================================================

-- | Compute positions for all nodes in a graph based on its block-cut tree decomposition.
layout :: forall node. Ord node => BlockCutConfig -> SimpleGraph node -> BlockCutLayout node
layout config graph =
  let
    w = config.width
    m = config.margin

    -- Decompose
    bct = blockCutTree graph
    aps = articulationPoints graph
    nBlocks = Array.length bct.blocks

    -- Build block-cut tree adjacency for BFS
    bctAdj = foldl (\acc edge ->
      Map.alter (Just <<< Set.insert edge.to <<< fromMaybe Set.empty) edge.from
        (Map.alter (Just <<< Set.insert edge.from <<< fromMaybe Set.empty) edge.to acc)
    ) Map.empty bct.tree

    -- Find largest block to root BFS from
    largestBlockIdx = fromMaybe 0 $ map fst_ $
      Array.head $ Array.sortBy (\a b -> compare (snd_ b) (snd_ a)) $
        mapWithIndex (\i block -> Tuple i (Set.size block)) bct.blocks

    -- BFS layers of block indices
    bfsLayers = bfsFromRoot bctAdj largestBlockIdx

    -- Classify blocks as bridge or not
    blockIsBridge = mapWithIndex (\_ block -> Set.size block <= 2) bct.blocks

    -- Compute radius for each block
    blockRadii = mapWithIndex (\_ block -> blockRadius config (Set.size block)) bct.blocks

    -- Content-driven layer positioning: stack layers by actual radii + gap
    blockPositions = positionBlocksCompact config bfsLayers nBlocks w m blockRadii

    -- Build block layout records
    blockLayouts = Map.fromFoldable $ Array.concatMap (\(Tuple depth layer) ->
      Array.mapMaybe (\idx ->
        case Map.lookup idx blockPositions of
          Nothing -> Nothing
          Just pos ->
            let
              nodeCount = fromMaybe 0 $ map Set.size (bct.blocks `Array.index` idx)
              radius = fromMaybe 0.0 (blockRadii `Array.index` idx)
              isBridge = fromMaybe false (blockIsBridge `Array.index` idx)
            in Just $ Tuple idx
              { x: pos.x, y: pos.y, radius, nodeCount, isBridge, depth }
      ) layer
    ) (mapWithIndex Tuple bfsLayers)

    -- Position nodes within their blocks
    nodePositions = foldl (\acc (Tuple blockIdx block) ->
      let
        center = fromMaybe { x: w / 2.0, y: 0.0 } (Map.lookup blockIdx blockPositions)
        blockNodes = Array.sort (Set.toUnfoldable block :: Array node)
        bn = Array.length blockNodes
        r = if bn <= 1 then 0.0 else fromMaybe 0.0 (blockRadii `Array.index` blockIdx)
      in foldl (\a (Tuple i nd) ->
        let
          angle = 2.0 * Number.pi * Int.toNumber i / Int.toNumber (max bn 1) - Number.pi / 2.0
          x = center.x + r * Number.cos angle
          y = center.y + r * Number.sin angle
        in Map.insert nd
          { x, y
          , block: blockIdx
          , isArticulationPoint: Set.member nd aps
          , isIsolated: false
          } a
      ) acc (mapWithIndex Tuple blockNodes)
    ) Map.empty (mapWithIndex Tuple bct.blocks)

    -- Find bottom of positioned blocks for isolated node placement
    blockBottom = foldl (\acc (Tuple idx bl) ->
      let r = fromMaybe 0.0 (blockRadii `Array.index` idx)
      in max acc (bl.y + r)
    ) 0.0 (Map.toUnfoldable blockPositions :: Array (Tuple Int { x :: Number, y :: Number }))

    -- Position isolated nodes just below the last block layer
    allPositioned = Map.keys nodePositions
    isolated = Array.filter (\nd -> not (Set.member nd allPositioned)) graph.nodes
    isolatedY = blockBottom + config.isolatedGap + 8.0
    allPositions = foldl (\acc (Tuple i nd) ->
      let
        x = m + Int.toNumber i * config.isolatedSpacing + 8.0
      in Map.insert nd
        { x, y: isolatedY
        , block: -1
        , isArticulationPoint: false
        , isIsolated: true
        } acc
    ) nodePositions (mapWithIndex Tuple isolated)

    -- Compute tight bounding box from all positioned content
    nodeExtent = foldl (\acc (Tuple _nd nl) ->
      let
        r = case Map.lookup nl.block blockLayouts of
              Just bl -> bl.radius
              Nothing -> 6.0
        labelPad = 14.0
      in { minX: min acc.minX (nl.x - r)
         , maxX: max acc.maxX (nl.x + r)
         , minY: min acc.minY (nl.y - r - labelPad)
         , maxY: max acc.maxY (nl.y + r + labelPad)
         }
    ) { minX: w, maxX: 0.0, minY: 1.0e6, maxY: -1.0e6 }
      (Map.toUnfoldable allPositions :: Array (Tuple node NodeLayout))

    -- Shift to origin + padding, derive tight dimensions
    pad = m / 2.0
    shiftX = -nodeExtent.minX + pad
    shiftY = -nodeExtent.minY + pad
    finalW = max w (nodeExtent.maxX - nodeExtent.minX + pad * 2.0)
    finalH = max config.minHeight (nodeExtent.maxY - nodeExtent.minY + pad * 2.0)

    shiftedNodes = map (\nl -> nl { x = nl.x + shiftX, y = nl.y + shiftY }) allPositions
    shiftedBlocks = map (\bl -> bl { x = bl.x + shiftX, y = bl.y + shiftY }) blockLayouts

  in
    { nodes: shiftedNodes
    , blocks: shiftedBlocks
    , width: finalW
    , height: finalH
    }

-- =============================================================================
-- Internal helpers
-- =============================================================================

-- | Compute block radius from node count
blockRadius :: BlockCutConfig -> Int -> Number
blockRadius config bn =
  min config.maxBlockRadius (max config.minBlockRadius (Number.sqrt (Int.toNumber bn) * config.blockRadiusScale))

-- | BFS from a root node, returning layers of node indices
bfsFromRoot :: Map.Map Int (Set.Set Int) -> Int -> Array (Array Int)
bfsFromRoot adj root = go [root] (Set.singleton root) []
  where
  go queue visited layers =
    if Array.length queue == 0 then layers
    else
      let
        nextQueue = Array.concatMap (\nd ->
          let nbrs = fromMaybe Set.empty (Map.lookup nd adj)
          in Array.filter (\nb -> not (Set.member nb visited))
               (Set.toUnfoldable nbrs :: Array Int)
        ) queue
        nextVisited = Array.foldl (flip Set.insert) visited nextQueue
      in go nextQueue nextVisited (Array.snoc layers queue)

-- | Assign block center positions by stacking layers from actual block radii.
-- | Each layer's Y = previous layer bottom + gap + this layer's max radius.
positionBlocksCompact :: BlockCutConfig -> Array (Array Int) -> Int -> Number -> Number -> Array Number -> Map.Map Int { x :: Number, y :: Number }
positionBlocksCompact config bfsLayers nBlocks w margin_ radii =
  let
    -- Compute max radius per layer
    layerMaxRadius layer =
      let blockIndices = Array.filter (\idx -> idx < nBlocks) layer
      in foldl (\acc idx -> max acc (fromMaybe 0.0 (radii `Array.index` idx))) 0.0 blockIndices
  in
    (Array.foldl (\{ positions, currentY } (Tuple _depth layer) ->
      let
        blockIndices = Array.filter (\idx -> idx < nBlocks) layer
        nInLayer = Array.length blockIndices
        maxR = layerMaxRadius layer
        layerY = currentY + maxR
        spacing = if nInLayer <= 1 then 0.0
                  else (w - margin_ * 2.0) / Int.toNumber (nInLayer - 1)
        newPositions = Array.foldl (\a (Tuple i blockIdx) ->
          let x = if nInLayer <= 1 then w / 2.0
                  else margin_ + Int.toNumber i * spacing
          in Map.insert blockIdx { x, y: layerY } a
        ) positions (mapWithIndex Tuple blockIndices)
        nextY = layerY + maxR + config.layerGap
      in { positions: newPositions, currentY: nextY }
    ) { positions: Map.empty, currentY: margin_ } (mapWithIndex Tuple bfsLayers)).positions

-- Tuple helpers (avoid importing Tuple accessors)
fst_ :: forall a b. Tuple a b -> a
fst_ (Tuple a _) = a

snd_ :: forall a b. Tuple a b -> b
snd_ (Tuple _ b) = b
