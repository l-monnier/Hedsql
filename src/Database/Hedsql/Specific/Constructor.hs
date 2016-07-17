{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
    ( ReturningState
    , returning
    ) where

import Control.Monad.State.Lazy

import Database.Hedsql.Common.AST

{-|
Create a RETURNING clause.

This clause is specific to PostgreSQL for INSERT, UPDATE and DELETE statements.
In MariaDB it can be used for DELETE statements only.
It is not available for SQLite.
-}
class ReturningState a b where
    returning :: ColRefWrap b -> State (a b) ()
