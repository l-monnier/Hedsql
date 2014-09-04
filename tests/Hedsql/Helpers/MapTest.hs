{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Hedsql.Helpers.MapTest where
import Hedsql.Helpers.Map

import Data.List
import Data.Map
--import System.Random
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
--import Test.QuickCheck.All
--import Test.QuickCheck.Gen
import qualified Data.List as List
import qualified Data.Map as Map

-- | Gather all tests.
test =
    testGroup "Helpers.Map" [
          testProperty "Deletes"                      prop_deletes
        , testProperty "Filter keys"                  prop_filterKeys
        , testProperty "Filter keys with keys"        prop_filterKeysWithKeys
        , testProperty "Filter keys with: idempotent" prop_filterKeysWithIde
        , testProperty "Filter keys maps unique: idempotent"
            prop_filterKeysMapsUniqueIde
        , testProperty "Filter keys maps unique first: idempotent"
            prop_filterKeysMapsUniqueFirstIde
        , testProperty "Filter keys maps unique ID"
            prop_filterKeysMapsUniqueId
        , testProperty "Group filter keys maps group keys"
            prop_groupFilterKeysMapsGroupKeys
        , testProperty "Key has value"                prop_keyHasValue
        , testProperty "Nub maps by ID: idempotent"   prop_nubMapsByIdIde
        , testProperty "Split maps: idempotent"       prop_splitMapsIde
        , testProperty "splitMapsBackToOrigin"        prop_splitMapsBackToOrigin
        ]

-- | Use different predicates randomly.
data Predicate = Predicate {
      name       :: String
    , predicate :: (SmallInt -> [SmallInt] -> Bool)
    }

instance Arbitrary Predicate where
    arbitrary =
        elements [
          Predicate "elem" elem
        , Predicate "notElem" notElem
        ]
    
instance Show Predicate where
    show = name

{-|
Using only small integers increases the chances of the existance of a key
in the map.
-}
newtype SmallInt = SmallInt Int
    deriving (Eq, Ord, Show)

instance Arbitrary SmallInt where
    arbitrary = do
        number <- choose (-20, 20)
        return $ SmallInt number

-- | Test that the deletes function behave as the filterKeysWith function.
prop_deletes::[SmallInt] -> [(SmallInt, SmallInt)] -> Bool
prop_deletes keys tuple = filterKeysWith notElem keys map == deletes keys map
    where
        map = fromList tuple

-- | Test that the filterKeys function behave as the filterKeysWith function.
prop_filterKeys::[SmallInt] -> [(SmallInt, SmallInt)] -> Bool
prop_filterKeys keys tuple = filterKeysWith elem keys map == filterKeys keys map
    where
        map = fromList tuple

{-|
Test the extract happens as planned, by testing the keys.
They keys which are present in the intial supplied map must also be found in the
filtered one. However, it does not mean they need to be in the same order.
Also, a duplicate key should be found only once.
-}
prop_filterKeysWithKeys::
       Predicate
    -> [SmallInt]
    -> [(SmallInt, SmallInt)]
    -> Bool
prop_filterKeysWithKeys predi keys tuple =
    keysInMap == keysInExtract
    where
        map = fromList tuple
        pred = predicate predi
        keysInMap =
            nub $ sort $ List.filter (\key -> pred key keys) $ Map.keys map
        keysInExtract = sort $ Map.keys $ filterKeysWith pred keys map
        
-- | Test the idempotent property of the filterKeysWith function.
prop_filterKeysWithIde::
       Predicate
    -> [SmallInt]
    -> [(SmallInt, SmallInt)]
    -> Bool
prop_filterKeysWithIde predi keys tuple =
    filterKeysWith pred keys (filterKeysWith pred keys map)
    == filterKeysWith pred keys map
    where
        map = fromList tuple
        pred = predicate predi

-- | Test the idempotent property of the filterKeysMapsUnique function.
prop_filterKeysMapsUniqueIde ::
       SmallInt
    -> [SmallInt]
    -> [[(SmallInt, SmallInt)]]
    -> Bool
prop_filterKeysMapsUniqueIde id keys tuples =
    func (func maps) == func maps
    where
        maps = List.map fromList tuples
        func = filterKeysMapsUniqueId id keys

-- | Test the idempotent property of the filterKeysMapsUniqueFirst function.
prop_filterKeysMapsUniqueFirstIde ::
       [SmallInt]
    -> [[(SmallInt, SmallInt)]]
    -> Bool
prop_filterKeysMapsUniqueFirstIde keys tuples =
    func (func maps) == func maps
    where
        maps = List.map fromList tuples
        func = filterKeysMapsUniqueFirst keys

{- |
If we extract only the ID values of the filtered keys, this list should be the
same as the one of the unique ID values of the complete list of maps.
-}
prop_filterKeysMapsUniqueId ::
       SmallInt
    -> [SmallInt]
    -> [[(SmallInt, SmallInt)]]
    -> Bool
prop_filterKeysMapsUniqueId id keys tuples =
    filterKeysValues == nubKeysValues
    where
        maps = List.map fromList tuples
        filterKeysValues =
            List.map (Map.lookup id) $ filterKeysMapsUniqueId id keys maps
        nubKeysValues = nub $ List.map (Map.lookup id) maps 

-- | Test that the keys of the returned map are the provided ones.
prop_groupFilterKeysMapsGroupKeys ::
    [(SmallInt, [SmallInt])]
    -> [[(SmallInt, SmallInt)]]
    -> Bool
prop_groupFilterKeysMapsGroupKeys groups tuples =
    (sort $ keys newMaps) == groupKeys
    where
         -- Nub function is used as duplicated keys are merged in a map.
         groupKeys = sort $ nub $ List.concatMap (\x -> [fst x]) groups
         maps = List.map fromList tuples
         newMaps = groupFilterKeysMaps groups maps

-- | Test the keyHasValue function litteraly.
prop_keyHasValue ::
       SmallInt
    -> SmallInt
    -> [(SmallInt, SmallInt)]
    -> Bool
prop_keyHasValue key value tuple =
    if       keyHasValue key value map
    then mapValue == Just value
    else mapValue /= Just value
    where
        map         = fromList tuple
        mapValue = Map.lookup key map

-- | Test the idempotent property of the nubMapsById function.
prop_nubMapsByIdIde ::
       SmallInt
    -> [[(SmallInt, SmallInt)]]
    -> Bool
prop_nubMapsByIdIde id tuples =
     nubMapsById id (nubMapsById id maps) == nubMapsById id maps
     where
         maps = List.map fromList tuples

{-|
Test the idempotent property of the splitMaps function on the first element of
the tuple.
-}
prop_splitMapsIde ::
       SmallInt
    -> [[(SmallInt, SmallInt)]]
    -> Bool
prop_splitMapsIde key tuples = 
    func (func maps) == func maps
    where
        maps = List.map fromList tuples
        func = fst . splitMaps key
 
-- | Make sure that the two elements of the tuple constitute the original list.
prop_splitMapsBackToOrigin ::
       SmallInt
    -> [[(SmallInt, SmallInt)]]
    -> Bool
prop_splitMapsBackToOrigin key tuples = 
    fst split ++ snd split == maps
    where
        maps = List.map fromList tuples
        split = splitMaps key maps