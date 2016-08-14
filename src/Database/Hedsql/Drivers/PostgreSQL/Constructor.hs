{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Database/Hedsql/Drivers/PostgreSQL/Constructor.hs
Description : PostgreSQL specific constructors.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL specific constructors for functions/clauses specific to this vendor.
-}
module Database.Hedsql.Drivers.PostgreSQL.Constructor
    ( default_
    , lateral
    , selectDistinctOn
    , returning
    ) where

import Control.Monad.State.Lazy

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Grammar
import Database.Hedsql.Common.Constructor
import Database.Hedsql.Specific.Constructor
import Database.Hedsql.Drivers.PostgreSQL.Driver

-- Public.

{-|
DEFAULT instruction when used to insert a DEFAULT value.
For example:
> INSERT INTO films Values (DEFAULT, 'Bananas', 88, '1971-07-13', 'Comedy');
-}
default_ :: Value a PostgreSQL
default_ = DefaultVal

-- | Create a sub-query preceded by LATERAL.
lateral ::
       SelectWrap PostgreSQL -- ^ Select query of the lateral clause.
    -> String                -- ^ Alias of the lateral clause.
    -> TableRef PostgreSQL   -- ^ Lateral table reference.
lateral s a = LateralRef s $ TableRefAs a []

{-|
Create a SELECT DISTINCT ON query.

This function is normally meant to be used for building a select query from
scratch, providing the ON clause and selected columns as arguments.
However, it is possible to apply it on an existing select query.
If that query is a single query, it will become a SELECT DISTINCT ON one.
If that query is a combination of select queries (UNION, EXCEPT, etc.) then
all the queries will become SELECT DISTINCT ON ones.
-}
selectDistinctOn ::
       SelectConstr a (Query c PostgreSQL)
    => [ColRefWrap PostgreSQL]             -- ^ Distinct expression.
    -> a                                   -- ^ Select clause.
    -> Query c PostgreSQL                  -- ^ Select query.
selectDistinctOn dExpr clause =
    modify (\_ -> setSelects selectType (DistinctOn dExpr) $ execStmt $ select clause)

{-|
Create a RETURNING clause for an INSERT statement specifically for PostgreSQL.
-}
instance ReturningConstr PostgreSQL InsertFromStmt where
    returning = returningGen

{-|
Create a RETURNING clause for an UPDATE statement specifically for PostgreSQL.
-}
instance ReturningConstr PostgreSQL UpdateWhereStmt where
    returning = returningGen

{-|
Create a RETURNING clause for a DELETE statement with only a FROM clause
specifically for PostgreSQL.
-}
instance ReturningConstr PostgreSQL DeleteFromStmt where
    returning = returningGen

{-|
Create a RETURNING clause for a DELETE statement with a WHERE clause
specifically for PostgreSQL.
-}
instance ReturningConstr PostgreSQL DeleteWhereStmt where
    returning = returningGen

{- TODO: delete

-- | Create a RETURNING clause for an UPDATE statement.
instance ReturningState Update PostgreSQL where
    returning =
        modify . set updateReturning . Just . Returning . coerce . selection
-}
