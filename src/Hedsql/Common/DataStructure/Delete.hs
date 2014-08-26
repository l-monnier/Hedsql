-- file : Hedsql/Common/DataStructure/Delete
{-# LANGUAGE TemplateHaskell #-}

{-|
    DELETE statement data type definitions.
-}

module Hedsql.Common.DataStructure.Delete where

import Hedsql.Common.DataStructure.Select
import Control.Lens

-- | DELETE query.
data Delete = Delete {
      _deleteTable :: Table
    , _deleteWherePart :: Maybe Where
} deriving (Show)

makeLenses ''Delete