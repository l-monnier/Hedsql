{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Commun/DataStructure/Update.hs
Description : Data structure of the UPDATE statement.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Data structure of the UPDATE statement.
-}
module Hedsql.Common.DataStructure.Update where

import Hedsql.Common.DataStructure.Select

import Control.Lens

-- | UPDATE query.
data Update a = Update
    { _updateTable :: Table a
    , _updateAssignments :: [Assignment a]
    , _updateWherePart :: Maybe (Where a)
    } deriving (Show)

-- | A value assigned to a column (used in the SET clause of an UPDATE statement).
data Assignment a = Assignment
    { _assignmentCol :: Column a
    , _assignmentVal :: Expression a
    } deriving (Show)

-- Make the lenses.
makeLenses ''Assignment
makeLenses ''Update