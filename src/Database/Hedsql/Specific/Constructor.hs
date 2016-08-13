{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Specific/Constructor.hs
Description : Constructors which are specific to more than one vendor.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructors for functions/clauses specific to more than one vendor.
-}
module Database.Hedsql.Specific.Constructor
    ( ReturningConstrGen
    , ReturningConstr
    , returningGen
    , returning
    , returningClause
    ) where

import Unsafe.Coerce

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.Grammar

{-|
Compose a RETURNING clause for a specific vendor.
-}
class ReturningConstr dbVendor a where
    returning ::
            ( SelectionConstr selection (Selection [colType] dbVendor)
            , ReturningConstrGen a b
            )
           => selection
           -> a dbVendor
           -> b colType dbVendor

{-|
Compose a RETURNING clause in a database vendor agnostic way.

This class is an internal machinery which later allow to build instances for
RETURNING clauses for a specific database vendor.
This way, only the instances supported by the vendor are implemented.
-}
class ReturningConstrGen a b | a -> b where
    returningGen ::
           SelectionConstr selection (Selection [colType] dbVendor)
        => selection -- ^ Reference to a column or list of columns.
        -> a dbVendor
        -> b colType dbVendor

-- | Create a RETURNING clause for a DELETE statement with only a FROM clause.
instance ReturningConstrGen DeleteFromStmt DeleteReturningStmt where
    returningGen ::
           SelectionConstr selection (Selection [colType] dbVendor)
        => selection -- ^ Reference to a column or list of columns.
        -> DeleteFromStmt dbVendor
        -> DeleteReturningStmt colType dbVendor
    returningGen = DeleteFromReturningStmt . returningClause


-- | Create a RETURNING clause for a DELETE statement with a WHERE clause.
instance ReturningConstrGen DeleteWhereStmt DeleteReturningStmt where
    returningGen ::
           SelectionConstr selection (Selection [colType] dbVendor)
        => selection -- ^ Reference to a column or list of columns.
        -> DeleteWhereStmt dbVendor
        -> DeleteReturningStmt colType dbVendor
    returningGen = DeleteWhereReturningStmt . returningClause

{-|
Create a RETURNING clause.

This clause is specific to PostgreSQL for INSERT, UPDATE and DELETE statements.
In MariaDB it can be used for DELETE statements only.
It is not available for SQLite.
-}
returningClause ::
       SelectionConstr selection (Selection [colType] dbVendor)
    => selection -- ^ Reference to a column or list of columns.
    -> Returning colType dbVendor
returningClause =
    Returning . coerce . selection
    where
        -- Coerce the phantom type to a single value.
        coerce :: Selection [colType] dbVendor -> Selection colType dbVendor
        coerce = unsafeCoerce
