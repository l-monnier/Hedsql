-- file : Hedsql/Common/DataStructure/Drop
{-# LANGUAGE TemplateHaskell #-}

{-|
    DROP statements data type definitions.
-}

module Hedsql.Common.DataStructure.Drop where

import Hedsql.Common.DataStructure.Select
import Control.Lens

-- | DROP TABLE statement.
data DropTable = DropTable {
      _dropTableIfExistsParam :: Bool
    , _dropTableTable :: Table
} deriving (Show)

-- | DROP VIEW statement.
data DropView = DropView {
     _dropViewName :: [Char]
}

-- Make the lenses.
makeLenses ''DropTable
makeLenses ''DropView