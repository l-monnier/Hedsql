{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/DataStructure/Insert.hs
Description : Constructor functions for columns.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

INSERT statement data type definitions.
-}
module Hedsql.Common.DataStructure.Insert where

import Hedsql.Common.DataStructure.Select

import Control.Lens

-- Private functions.

-- Public functions.

-- | INSERT query.
data Insert a = Insert
    { _insertTable   :: Table a
    , _insertColumns :: Maybe [Column a]
    , _insertValues  :: [[SqlValue a]]
    } deriving (Show)

-- Make the lenses.
makeLenses ''Insert