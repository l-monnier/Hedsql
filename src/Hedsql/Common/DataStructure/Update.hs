-- file : Hedsql/Common/DataStructure/Update
{-# LANGUAGE TemplateHaskell #-}

{-|
    Update statement data type definitions.
-}

module Hedsql.Common.DataStructure.Update where

import Hedsql.Common.DataStructure.Select
import Control.Lens

-- | UPDATE query.
data Update = Update {
      _updateTable :: Table
    , _updateAssignments :: [Assignment]
    , _updateWherePart :: Maybe Where
} deriving (Show)

-- | A value assigned to a column (used in the SET clause of an UPDATE statement).
data Assignment = Assignment {
      _assignmentCol :: Column
    , _assignmentVal :: Expression
} deriving (Show)

-- Make the lenses.
makeLenses ''Assignment
makeLenses ''Update