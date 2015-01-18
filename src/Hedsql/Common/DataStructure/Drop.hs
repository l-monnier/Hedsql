{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/DataStructure/Drop.hs
Description : DROP statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

DROP statements data type definitions.
-}
module Hedsql.Common.DataStructure.Drop where

import Hedsql.Common.DataStructure.Select

import Control.Lens

-- Private functions.

-- Public functions.

-- | DROP TABLE statement.
data DropTable a = DropTable
    { _dropTableIfExistsParam :: Bool
    , _dropTableTable         :: Table a
    } deriving (Show)

-- | DROP VIEW statement.
data DropView a = DropView
    { _dropViewName :: [Char]
    } deriving (Show)

-- Make the lenses.
makeLenses ''DropTable
makeLenses ''DropView