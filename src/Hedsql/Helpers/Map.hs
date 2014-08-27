-- file : Hedsql/Helpers/Map.hs

module Hedsql.Helpers.Map where

import Data.List
import Data.Map
import qualified Data.List as List
import qualified Data.Map as Map

-- | Delete the provided list of keys from a map.
deletes:: Ord k =>
      [k] -- ^ Keys to delete from the map.
    -> Map k a
    -> Map k a
deletes = filterKeysWith Prelude.notElem

-- | Return a map containing only the provided keys (if they exists).
filterKeys :: Ord k =>
      [k] -- ^ Keys to be returned in the new map.
    -> Map k a
    -> Map k a
filterKeys = filterKeysWith Prelude.elem

{- |
    Return a map containing only the keys that satisfy the predicate
    (and their associated value).
-}
filterKeysWith:: Ord k => (k -> [k] -> Bool) -> [k] -> Map k a -> Map k a
filterKeysWith predicate keys = Map.filterWithKey (\k _ -> predicate k keys)

{-|
    Extract the provided keys from a list of maps and keep only the instances
    with a distinct first value.
    If an empty list of keys is provided, the function will return a list of
    empty maps.
-}
filterKeysMapsUniqueFirst :: (Eq a, Ord k) => [k] -> [Map k a] -> [Map k a]
filterKeysMapsUniqueFirst [] maps =
    List.map (\map -> empty) maps
filterKeysMapsUniqueFirst (key:keys) maps =
    filterKeysMapsUniqueId key (key:keys) maps

{-|
    Extract the provided keys from a list of maps and keep only the instances
    with a distinct ID.
    To ensure the idempotent property of the function, the ID will always be
    considered as part of the list of keys to extract.
-}
filterKeysMapsUniqueId :: (Eq a, Ord k) => k -> [k] -> [Map k a] -> [Map k a]
filterKeysMapsUniqueId id keys maps =
    List.map (filterKeys (List.union keys [id])) $ nubMapsById id maps
 
{-|
    Apply many filterKeys using different keys and group each extract in a new
    map with a specified key for each group.
-}
groupFilterKeysMaps :: (Eq a, Ord k, Ord k1) => [(k1, [k])] -> [Map k a] -> Map k1 [Map k a]
groupFilterKeysMaps [] maps = empty
groupFilterKeysMaps (groupKey:groupKeys) maps =
    Map.union extract $ groupFilterKeysMaps groupKeys maps
    where
        extract = singleton (fst groupKey) keys
        keys = filterKeysMapsUniqueFirst (snd groupKey) maps

{-|
    Return true if the specified value is equal to the one matching the provided
    key in the map.
    If the key does not exist, return false.
-}
keyHasValue :: (Eq a, Ord k) => k -> a -> Map k a -> Bool
keyHasValue key value map =
    case Map.lookup key map of
       Nothing -> False
       Just result -> if result == value then True else False

{-|
    Keep only the maps with a distinct ID.
    If the provided ID is not a key of one of the provided map the tested value
    will be Nothing.
    This means that if the provided ID does not exist in any map, only the
    first map will be returned.
    If the provided ID exists in one map only, this map will be part of the
    returned list of maps.
-}
nubMapsById :: (Eq a, Ord k) => k -> [Map k a] -> [Map k a]
nubMapsById id maps =
    nubBy (\map1 map2 -> Map.lookup id map1 == Map.lookup id map2) maps
      
-- | Split a list of maps as soon as the value of a given key changes.
splitMaps:: (Eq a, Ord k) => k -> [Map k a] -> ([Map k a], [Map k a])
splitMaps key [] = ([], [])
splitMaps key maps =
    case Map.lookup key $ head maps of
        Nothing -> (maps, [])
        Just value -> break (\map -> keyHasValue key value map == False) maps