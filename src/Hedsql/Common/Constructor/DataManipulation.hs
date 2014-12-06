{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Hedsql.Helpers.Coerce

-- private functions.

-- public functions.

-- | Create a column/value pair to be used in an UPDATE statement.
assign ::
    (
       Coerce b [Column a]
    ,  Coerce a (SqlValue a)
    )
    => b -- ^ Column or name of the column.
    -> a -- ^ Value for this column.
    -> Assignment a
assign c val = Assignment (column c) (ValueExpr $ value val)

-- | Create a DELETE FROM statement.
deleteFrom ::
       Coerce a (Table a)
    => a -- ^ Table or name of the table to delete from.
    -> Delete a
deleteFrom t = Delete (table t) Nothing

{-|
Create an INSERT INTO statement.

The values to insert are a list of list of values because you may insert more
than one row in the database.
-}
insertInto ::
    (
       Coerce b (Table a)
    ,  Coerce [a] [SqlValue a]
    )
    => b      -- ^ Table or name of the table to insert the data into.
    -> [[a]]  -- ^ Values to insert.
    -> Insert a
insertInto t vals = Insert (table t) Nothing $ map values vals

{-|
Create an INSERT INTO statement where the columns are specified.

The values to insert are a list of list of values because you may insert more
than one row in the database.
-}
insertIntoCols ::
    (
       Coerce [a] [SqlValue a]
    ,  Coerce c [Column a]
    ,  Coerce b (Table a)
    )
    => b     -- ^ Table or name of the table to insert the data into.
    -> [c]   -- ^ Columns or names of the columns.
    -> [[a]] -- ^ Values to insert.
    -> Insert a
insertIntoCols t cols vals =
    Insert (table t) (Just $ map column cols) $ map values vals

-- | Create an UPDATE statement.
update ::
       Coerce a (Table a)
    => a              -- ^ Table to update.
    -> [Assignment a] -- ^ Column/value assignements.
    -> Update a
update t assignments = Update (table t) assignments Nothing