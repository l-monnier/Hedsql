{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Database/Hedsql/Common/DataStructure/Drop.hs
Description : DROP statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

DROP statements data type definitions.
-}
module Database.Hedsql.Common.DataStructure.Drop where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.DataStructure.Select

import Control.Lens

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | DROP TABLE statement.
data DropTable a = DropTable
    { _dropTableIfExistsParam :: Bool
    , _dropTableTable         :: Table a
    }

-- | DROP VIEW statement.
data DropView a = DropView
    { _dropViewName :: [Char]
    }

-- Make the lenses.
makeLenses ''DropTable
makeLenses ''DropView