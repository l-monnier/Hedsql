{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Database/Hedsql/Drivers/MariaDB/Constructor.hs
Description : MariaDB specific constructors.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

MariaDB specific constructors for functions/clauses specific to this vendor.
-}
module Database.Hedsql.Drivers.MariaDB.Constructor
    ( calcFoundRows
    , foundRows
    ) where

import Control.Lens hiding (assign, from)
import Control.Monad.State.Lazy

import Database.Hedsql.Common.AST
import Database.Hedsql.Specific.Constructor
import Database.Hedsql.Drivers.MariaDB.Driver

-- Public.

-- | SQL_CALC_FOUND_ROWS function.
calcFoundRows :: Expression MariaDB Void
calcFoundRows = CalcFoundRows

-- | FOUND_ROWS function.
foundRows :: Expression MariaDB Int
foundRows = FoundRows

-- | Create a RETURNING clause for a DELETE statement.
instance ReturningState Delete MariaDB where
    returning = modify . set deleteReturning . Just . Returning
