{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Database/Hedsql/Common/Constructor/DataManipulation.hs
Description : DELETE, INSERT and UPDATE SQL statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SQL data manipulation queries constructors for DELETE, INSERT and UPDATE.
-}
module Database.Hedsql.Common.Constructor.DataManipulation
    ( assign
    , deleteFrom
    , insertInto
    , insertIntoCols
    , update
    ) where
    
import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.Constructor.Tables
import Database.Hedsql.Common.Constructor.Values
import Database.Hedsql.Common.DataStructure

-- private functions.

-- public functions.

-- | Create a column/value pair to be used in an UPDATE statement.
assign ::
    (
       ToCols      a [Column   c]
    ,  ToSqlValues b [SqlValue c]
    )
    => a -- ^ Column or name of the column.
    -> b -- ^ Value for this column.
    -> Assignment c
assign a val = Assignment (toCol a) (ValueExpr $ value val)

-- | Create a DELETE FROM statement.
deleteFrom ::
       ToTables a [Table b]
    => a        -- ^ Table or name of the table to delete from.
    -> Delete b
deleteFrom t = Delete (table t) Nothing

{-|
Create an INSERT INTO statement.

The values to insert are a list of list of values because you may insert more
than one row in the database.
-}
insertInto ::
    (
       ToTables    a   [Table    c]
    ,  ToSqlValues [b] [SqlValue c]
    )
    => a        -- ^ Table or name of the table to insert the data into.
    -> [[b]]    -- ^ Values to insert.
    -> Insert c
insertInto t vals = Insert (table t) Nothing $ map values vals

{-|
Create an INSERT INTO statement where the columns are specified.

The values to insert are a list of list of values because you may insert more
than one row in the database.
-}
insertIntoCols ::
    (
      ToTables    a   [Table d]
    , ToCols      b   [Column d]
    , ToSqlValues [c] [SqlValue d]
    )
    => a     -- ^ Table or name of the table to insert the data into.
    -> [b]   -- ^ Columns or names of the columns.
    -> [[c]] -- ^ Values to insert.
    -> Insert d
insertIntoCols t cs vals =
    Insert (table t) (Just $ map toCol cs) $ map values vals

-- | Create an UPDATE statement.
update ::
       ToTables a [Table b]
    => a              -- ^ Table to update.
    -> [Assignment b] -- ^ Column/value assignements.
    -> Update b
update t assignments = Update (table t) assignments Nothing