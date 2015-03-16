{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Database/Hedsql/Commun/DataStructure/Update.hs
Description : Data structure of the UPDATE statement.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Data structure of the UPDATE statement.
-}
module Database.Hedsql.Common.DataStructure.Update where 

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.DataStructure.Select

import Control.Lens

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
A value assigned to a column (used in the SET clause of an UPDATE statement).
-}
data Assignment a where
    Assignment :: Column b a -> Expression b a -> Assignment a

-- | UPDATE query.
data Update a = Update
    { _updateTable       :: Table a
    , _updateAssignments :: [Assignment a]
    , _updateWherePart   :: Maybe (Where a)
    }

-- Make the lenses.
makeLenses ''Assignment
makeLenses ''Update