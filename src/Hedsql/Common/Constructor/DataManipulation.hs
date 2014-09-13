{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Hedsql/Common/Constructor/DataManipulation.hs
Description : DELETE, INSERT and UPDATE SQL statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SQL data manipulation queries constructors for DELETE, INSERT and UPDATE.
-}
module Hedsql.Common.Constructor.DataManipulation
    (
      assign
    , deleteFrom
    , insertInto
    , insertIntoCols
    , update
    ) where

import Hedsql.Common.Constructor.Columns
import Hedsql.Common.Constructor.Tables
import Hedsql.Common.Constructor.Values
import Hedsql.Common.DataStructure.Base

-- private functions.

-- | Convert a value so it can be assigned in an UPDATE statement.
class AssignedValueConstruct a where
    toAssignedVal :: a -> Expression
    toAssignedVals :: a -> [Expression]
    
instance ColRefConstruct a => AssignedValueConstruct a where
    toAssignedVal = toExpr
    toAssignedVals a = [toExpr a]

instance ColRefConstruct a => AssignedValueConstruct [a] where
    toAssignedVal = toAssignedVal.head
    toAssignedVals = map toAssignedVal

instance AssignedValueConstruct String where
    toAssignedVal a = ValueExpr (toValue a)
    toAssignedVals a = [toAssignedVal a]

instance AssignedValueConstruct [String] where
    toAssignedVal = toAssignedVal.head
    toAssignedVals = map toAssignedVal

-- public functions.

-- | Create a column/value pair to be used in an UPDATE statement.
assign ::
    (ColumnConstructor a, AssignedValueConstruct b) => a -> b -> Assignment
assign c value = Assignment (column c) (toAssignedVal value)

-- | Create a DELETE FROM statement.
deleteFrom :: TableConstructor a => a -> Delete
deleteFrom t = Delete (table t) Nothing

-- | Create an INSERT INTO statement.
insertInto ::
      (TableConstructor a, SqlValueConstructor b)
    => a -> [b] -> Insert
insertInto t values = Insert (table t) Nothing $ map toValues values

-- | Create an INSERT INTO statement where the columns are specified.
insertIntoCols ::
      (TableConstructor a, ColumnConstructor b, SqlValueConstructor c)
    => a -> [b] -> [c] -> Insert
insertIntoCols t cols values =
    Insert (table t) (Just $ map column cols) $ map toValues values

-- | Create an UPDATE statement.
update :: TableConstructor a => a -> [Assignment] -> Update
update t assignments = Update (table t) assignments Nothing