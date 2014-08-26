-- file : Hedsql/common/dataStructure/insert
{-# LANGUAGE TemplateHaskell #-}

{-|
    INSERT statement data type definitions.
-}

module Hedsql.Common.DataStructure.Insert where

import Hedsql.Common.DataStructure.Select
import Control.Lens

-- | INSERT query.
data Insert = Insert {
      _insertTable :: Table
    , _insertColumns :: Maybe [Column]
    , _insertValues :: [[SqlValue]]
}

-- Make the lenses.
makeLenses ''Insert