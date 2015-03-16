{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs           #-}

{-|
Module      : Database/Hedsql/Common/DataStructure/Insert.hs
Description : Constructor functions for columns.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

INSERT statement data type definitions.
-}
module Database.Hedsql.Common.DataStructure.Insert where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.DataStructure.Select

import Control.Lens

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- TODO: rework it so insert cols becomes assignments and the values can type
-- check against the columns types.

-- | INSERT statement.
data Insert a = Insert
    { _insertTable :: Table a
    
      -- ^ Columns where the values have to be inserted.
    , _insertCols  :: Maybe [ColWrap a]
    
      -- ^ Values to insert.
    , _insertValues :: [[ValueWrap a]]
    }
    
makeLenses ''Insert