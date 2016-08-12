{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
    , returning
    ) where

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.Grammar
import Database.Hedsql.Specific.Constructor
import Database.Hedsql.Drivers.MariaDB.Driver

-- Public.

-- | SQL_CALC_FOUND_ROWS function.
calcFoundRows :: Expression MariaDB Void
calcFoundRows = CalcFoundRows

-- | FOUND_ROWS function.
foundRows :: Expression MariaDB Int
foundRows = FoundRows

-- TODO: find a way to constraint the instances specifically to MariaDB.

-- | Create a RETURNING clause for a DELETE statement with only a FROM clause.
instance ReturningConstr DeleteFromStmt DeleteReturningStmt where
    returning ::
           SelectionConstr selection (Selection [colType] dbVendor)
        => selection -- ^ Reference to a column or list of columns.
        -> DeleteFromStmt dbVendor
        -> DeleteReturningStmt colType dbVendor
    returning = DeleteFromReturningStmt . returningClause

-- | Create a RETURNING clause for a DELETE statement with a WHERE clause.
instance ReturningConstr DeleteWhereStmt DeleteReturningStmt where
    returning ::
           SelectionConstr selection (Selection [colType] dbVendor)
        => selection -- ^ Reference to a column or list of columns.
        -> DeleteWhereStmt dbVendor
        -> DeleteReturningStmt colType dbVendor
    returning = DeleteWhereReturningStmt . returningClause
