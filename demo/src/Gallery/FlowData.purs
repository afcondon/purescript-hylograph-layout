-- | Gallery Flow Data Loading
-- |
-- | Loads data for flow visualizations: Sankey, Chord, EdgeBundle, Adjacency
-- | Converts CSV formats to algorithm-specific data structures
module Gallery.FlowData
  ( -- Sankey
    loadSankeyData
  , SankeyData
    -- Chord / Adjacency (same matrix format)
  , loadMatrixData
  , MatrixData
    -- EdgeBundle
  , loadEdgeBundleData
  , EdgeBundleData
  , EdgeBundleNode
  ) where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (concat, filter, foldl, length, mapMaybe, nub, sortBy, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), split, trim)
import Data.String.Common (null) as String
import Data.Tuple (Tuple(..))
import DataViz.Layout.Sankey.Types (LinkCSVRow)
import Effect.Aff (Aff)

-- =============================================================================
-- Sankey Data
-- =============================================================================

-- | Sankey data ready for layout
type SankeyData =
  { links :: Array LinkCSVRow
  }

-- | Load and parse energy.csv for Sankey diagram
loadSankeyData :: Aff (Either String SankeyData)
loadSankeyData = do
  response <- AJAX.get ResponseFormat.string "./data/energy.csv"
  case response of
    Left err -> pure $ Left $ "Failed to load energy data: " <> AJAX.printError err
    Right { body } ->
      let links = parseSankeyCSV body
      in pure $ Right { links }

-- | Parse CSV with source,target,value format
parseSankeyCSV :: String -> Array LinkCSVRow
parseSankeyCSV csvString =
  let
    lines = split (Pattern "\n") csvString
    dataLines = filter (not <<< isEmpty) $ Array.drop 1 lines
  in
    mapMaybe parseSankeyLine dataLines
  where
  isEmpty s = String.null (trim s)

parseSankeyLine :: String -> Maybe LinkCSVRow
parseSankeyLine line =
  let parts = map trim $ split (Pattern ",") line
  in case parts of
    [ s, t, valueStr ] ->
      case Number.fromString valueStr of
        Just v -> Just { s, t, v }
        Nothing -> Nothing
    _ -> Nothing

-- =============================================================================
-- Matrix Data (for Chord and Adjacency)
-- =============================================================================

-- | Matrix data with node names
type MatrixData =
  { matrix :: Array (Array Number)
  , names :: Array String
  }

-- | Load bridges.csv and convert to matrix format
-- | bridges.csv has From;To;Value format (semicolon separated!)
loadMatrixData :: Aff (Either String MatrixData)
loadMatrixData = do
  response <- AJAX.get ResponseFormat.string "./data/bridges.csv"
  case response of
    Left err -> pure $ Left $ "Failed to load bridges data: " <> AJAX.printError err
    Right { body } ->
      pure $ Right $ edgeListToMatrix body

-- | Convert edge list CSV to square matrix
edgeListToMatrix :: String -> MatrixData
edgeListToMatrix csvString =
  let
    lines = split (Pattern "\n") csvString
    dataLines = filter (not <<< isEmpty) $ Array.drop 1 lines
    edges = mapMaybe parseEdgeLine dataLines

    -- Extract unique names in sorted order
    allNames = nub $ concat $ map (\e -> [e.from, e.to]) edges
    names = sortBy compare allNames
    n = length names

    -- Create name to index mapping
    nameToIndex :: Map String Int
    nameToIndex = Map.fromFoldable $ Array.mapWithIndex (\i name -> Tuple name i) names

    -- Initialize empty matrix
    emptyMatrix = Array.replicate n (Array.replicate n 0.0)

    -- Fill matrix with edge values
    matrix = foldl (addEdge nameToIndex n) emptyMatrix edges
  in
    { matrix, names }
  where
  isEmpty s = String.null (trim s)

type Edge = { from :: String, to :: String, value :: Number }

parseEdgeLine :: String -> Maybe Edge
parseEdgeLine line =
  let parts = map trim $ split (Pattern ";") line
  in case parts of
    [ from, to, valueStr ] ->
      case Number.fromString valueStr of
        Just value -> Just { from, to, value }
        Nothing -> Nothing
    _ -> Nothing

-- | Add an edge to the matrix
addEdge :: Map String Int -> Int -> Array (Array Number) -> Edge -> Array (Array Number)
addEdge nameToIndex n matrix edge =
  case Map.lookup edge.from nameToIndex, Map.lookup edge.to nameToIndex of
    Just fromIdx, Just toIdx ->
      updateMatrix fromIdx toIdx edge.value matrix
    _, _ -> matrix

-- | Update a cell in the matrix
updateMatrix :: Int -> Int -> Number -> Array (Array Number) -> Array (Array Number)
updateMatrix row col value matrix =
  case matrix !! row of
    Just rowArray ->
      let newRow = Array.updateAt col value rowArray
      in case newRow of
        Just r -> fromMaybe matrix $ Array.updateAt row r matrix
        Nothing -> matrix
    Nothing -> matrix

-- =============================================================================
-- EdgeBundle Data
-- =============================================================================

-- | Node with imports for edge bundling
type EdgeBundleNode =
  { name :: String
  , imports :: Array String
  }

-- | EdgeBundle data ready for layout
type EdgeBundleData =
  { nodes :: Array EdgeBundleNode
  }

-- | Load concept-graph CSVs and convert to EdgeBundle format
loadEdgeBundleData :: Aff (Either String EdgeBundleData)
loadEdgeBundleData = do
  nodesResp <- AJAX.get ResponseFormat.string "./data/concept-graph-nodes.csv"
  edgesResp <- AJAX.get ResponseFormat.string "./data/concept-graph-edges.csv"
  case nodesResp, edgesResp of
    Left err, _ -> pure $ Left $ "Failed to load nodes: " <> AJAX.printError err
    _, Left err -> pure $ Left $ "Failed to load edges: " <> AJAX.printError err
    Right { body: nodesBody }, Right { body: edgesBody } ->
      pure $ Right $ buildEdgeBundleData nodesBody edgesBody

-- | Build EdgeBundle nodes from CSVs
buildEdgeBundleData :: String -> String -> EdgeBundleData
buildEdgeBundleData nodesCSV edgesCSV =
  let
    -- Parse node names
    nodeLines = filter (not <<< isEmpty) $ Array.drop 1 $ split (Pattern "\n") nodesCSV
    nodeNames = mapMaybe parseNodeLine nodeLines

    -- Parse edges
    edgeLines = filter (not <<< isEmpty) $ Array.drop 1 $ split (Pattern "\n") edgesCSV
    edges = mapMaybe parseGraphEdge edgeLines

    -- Build imports map: for each target, collect sources
    importsMap :: Map String (Array String)
    importsMap = foldl addImport Map.empty edges

    -- Create nodes with imports
    nodes = map (\name ->
      { name
      , imports: fromMaybe [] $ Map.lookup name importsMap
      }) nodeNames
  in
    { nodes }
  where
  isEmpty s = String.null (trim s)

parseNodeLine :: String -> Maybe String
parseNodeLine line =
  let parts = map trim $ split (Pattern ",") line
  in Array.head parts

type GraphEdge = { source :: String, target :: String }

parseGraphEdge :: String -> Maybe GraphEdge
parseGraphEdge line =
  let parts = map trim $ split (Pattern ",") line
  in case parts of
    [ source, target, _ ] -> Just { source, target }
    [ source, target ] -> Just { source, target }
    _ -> Nothing

-- | Add an edge to the imports map (target imports source)
addImport :: Map String (Array String) -> GraphEdge -> Map String (Array String)
addImport m edge =
  let existing = fromMaybe [] $ Map.lookup edge.target m
  in Map.insert edge.target (Array.snoc existing edge.source) m
