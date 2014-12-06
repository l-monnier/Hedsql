{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/DataStructure/Delete.hs
Description : DELETE statement data type definition.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

DELETE statement data type definitions.
-}
module Hedsql.Common.DataStructure.Delete
    (
      Delete (Delete)
    , deleteTable
    , deleteWhere
    ) where

import Hedsql.Common.DataStructure.Select
import Control.Lens

-- Private.

-- Public.

-- | DELETE statement.
data Delete a = Delete
    {
      _deleteTable :: Table a
    , _deleteWhere :: Maybe (Where a)
    } deriving (Show)

-- Make the lenses.
makeLenses ''Delete