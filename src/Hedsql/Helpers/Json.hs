-- file : Hedsql/Helpers/Json.hs
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hedsql.Helpers.Json where

import Control.Applicative (pure)
import Data.Text.Encoding (encodeUtf8)
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.ByteString.Char8
import Data.Map
import Data.Maybe
import Data.String
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Word
import Database.HDBC
import Data.Hashable
import Hedsql.Helpers.Map
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM

{-|
    ToJSON instance for HDBC SqlValue.
    Note that ByteString have been re-implemented - they were suppressed from
    Aeson since version 0.7 - to deal with all the SqlValue constructors.
-}     
instance FromJSON ByteString where
    parseJSON (String t) = pure . encodeUtf8 $ t
    parseJSON v          = typeMismatch "ByteString" v

instance ToJSON ByteString where
    toJSON = toJSON.Data.ByteString.Char8.unpack

$(deriveJSON defaultOptions ''Day)
$(deriveJSON defaultOptions ''NominalDiffTime)
$(deriveJSON defaultOptions ''LocalTime)
$(deriveJSON defaultOptions ''TimeOfDay)
$(deriveJSON defaultOptions ''TimeZone)
$(deriveJSON defaultOptions ''SqlValue)

{-|
    Extract the string content from a JSON object.
-}
getContent :: Object -> Text
getContent jsonValue =
        getSuccess result
    where
        result = fromJSON $ fromJust $ HM.lookup "contents" jsonValue
        getSuccess (Success a) = a :: Text

{-|
    Extract the content of a given key of a JSON object if the key exists.
-}
lookupContent :: Text -> Value -> Maybe Text
lookupContent k (Object v) =
    case (HM.lookup k v) of
        Nothing -> Nothing
        Just (Object value) -> Just $ getContent value    
    
{-|
    Convert a "flat" structure to an array of nested JSON values.
    
    This would be the result of the view in the database. We can see that some
    parts of some lines are duplicated. Daniel Lord appears twice because he is
    at the same time an actor and a singer.
    
    ------------------------------------------------------
    | id | firstName   | lastName | tags_id | tags_value |
    ------------------------------------------------------
    | 1  | Jacky       | Johnes   | 1       | actor      | 
    ------------------------------------------------------
    | 2  | Daniel      | Lord     | 1       | actor      |
    ------------------------------------------------------
    | 2  | Daniel      | Lord     | 2       | singer     |
    ------------------------------------------------------
    
    The use of the flatToNestedJSON is to transform this flat structure to a
    nested one.
-}
flatToNestedJSON :: (Eq a, Ord k, ToJSON a, ToJSON (Map k a))
    => k           -- ^ Name of the key which can be used as unique ID of the maps. 
    -> [(k, [k])] -- ^ Tuples where the first value is the name of the key of a nested value.
                     -- ^ The second value is the values which will be part of the nested value.
    -> [Map k a]
    -> [Map k Value]
flatToNestedJSON id fields [] = []
flatToNestedJSON id fields (map:maps) =
    [Map.union root tree] ++ (flatToNestedJSON id fields second)
    where
       (first, second) = splitMaps id (map:maps)
       root   = Map.map toJSON $ deletes (List.concatMap snd fields) map
       tree   = Map.map toJSON $ groupFilterKeysMaps fields first