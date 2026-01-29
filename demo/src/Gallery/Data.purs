-- | Gallery Data Loading
-- |
-- | Loads flare-2 hierarchical data for layout demonstrations
-- | Each node includes a unique path for coordinated highlighting
module Gallery.Data
  ( FlareNode
  , loadFlareData
  , flareToTree
  , flareToPackHierarchy
  , flareToPartitionHierarchy
  ) where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import Effect.Aff (Aff)

-- | Flare JSON node (blessed foreign data)
foreign import data FlareNode :: Type

-- | Parse JSON string into FlareNode hierarchy
foreign import parseFlareJson :: String -> FlareNode

-- | Get node name
foreign import getName :: FlareNode -> String

-- | Get node value (for leaves)
foreign import getValue :: FlareNode -> Number

-- | Get children (nullable for leaves)
foreign import getChildren_ :: FlareNode -> Nullable (Array FlareNode)

-- | Get children as Maybe
getChildren :: FlareNode -> Maybe (Array FlareNode)
getChildren = toMaybe <<< getChildren_

-- | Load flare data from assets
loadFlareData :: Aff (Either String FlareNode)
loadFlareData = do
  response <- AJAX.get ResponseFormat.string "./data/flare-2.json"
  case response of
    Left err -> pure $ Left $ "Failed to load flare data: " <> AJAX.printError err
    Right { body } -> pure $ Right $ parseFlareJson body

-- | Convert FlareNode to Data.Tree for Tree/Cluster layouts
-- | Tree nodes include: { name, path, value, x, y, depth, height }
-- | The path field is the full hierarchical path (e.g., "flare.animate.Tween")
flareToTree :: FlareNode -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
flareToTree node = go "" node
  where
  go :: String -> FlareNode -> Tree { name :: String, path :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
  go parentPath n =
    let
      name = getName n
      path = if parentPath == "" then name else parentPath <> "." <> name
      value = getValue n
      childrenMaybe = getChildren n
      childrenList = case childrenMaybe of
        Nothing -> Nil
        Just childrenArray -> fromFoldable $ map (go path) childrenArray
    in
      mkTree { name, path, value, x: 0.0, y: 0.0, depth: 0, height: 0 } childrenList

-- | Convert FlareNode to Pack.HierarchyData for Pack layout
-- | Includes path for coordinated highlighting
flareToPackHierarchy :: FlareNode -> Pack.HierarchyData { name :: String, path :: String }
flareToPackHierarchy node = go "" node
  where
  go :: String -> FlareNode -> Pack.HierarchyData { name :: String, path :: String }
  go parentPath n =
    let
      name = getName n
      path = if parentPath == "" then name else parentPath <> "." <> name
      value = getValue n
      childrenMaybe = getChildren n
      kids = case childrenMaybe of
        Nothing -> Nothing
        Just childrenArray -> Just $ map (go path) childrenArray
    in
      Pack.HierarchyData
        { data_: { name, path }
        , value: if value > 0.0 then Just value else Nothing
        , children: kids
        }

-- | Convert FlareNode to Partition.HierarchyData for Sunburst/Icicle
-- | Includes path for coordinated highlighting
flareToPartitionHierarchy :: FlareNode -> Partition.HierarchyData { name :: String, path :: String }
flareToPartitionHierarchy node = go "" node
  where
  go :: String -> FlareNode -> Partition.HierarchyData { name :: String, path :: String }
  go parentPath n =
    let
      name = getName n
      path = if parentPath == "" then name else parentPath <> "." <> name
      value = getValue n
      childrenMaybe = getChildren n
      kids = case childrenMaybe of
        Nothing -> Nothing
        Just childrenArray -> Just $ map (go path) childrenArray
    in
      Partition.HierarchyData
        { data_: { name, path }
        , value: if value > 0.0 then Just value else Nothing
        , children: kids
        }
