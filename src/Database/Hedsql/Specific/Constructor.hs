{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
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
    ( ReturningConstr
    , returning
    , returningClause
    ) where

import Unsafe.Coerce

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor

{-|
Compose a Returning clause.
-}
class ReturningConstr a b | a -> b where
    returning ::
           SelectionConstr selection (Selection [colType] dbVendor)
        => selection -- ^ Reference to a column or list of columns.
        -> a dbVendor
        -> b colType dbVendor

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
