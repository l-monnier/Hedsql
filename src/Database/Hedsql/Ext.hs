{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Ext.hs
Description : Untyped Hedsql.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Extension of Hedsql which allow to use untyped columns and values.
This extension allows some quick & dirty coding by not having to define
the type of the columns to perform. The downside is that you will not benefit
from static type checks.
However, this isn't a black or white choice as you can mix both styles.

= Example

The below example use directly a 'String' in the 'select' clause and neither
age nor the 'Int' value have a proper type: age is 'Undefined' while the value
is generic.
> select "firstName" /++ from "People" /++ where_ ("age" /> genVal (18::Int))

= Deactivate the GHC OverloadedStrings language extension

If you wish to provide parameters as simple 'String', if activated,
you should turn off the GHC OverloadedStrings language extension.

> LANGUAGE NoOverloadedStrings

This will allow to write queries like this:

> select "col1" /++ from "table1"

instead of:

> select ("col1"::String) /++ from ("table1"::String)
-}
module Database.Hedsql.Ext
    (
      -- * Table and table reference
      {-|
      Extra instances providing shortcuts such the ability to convert a
      table reference back to a table or creating tables directly from 'String'.
      -}
      ToTable
    , ToTableRef

      -- * Column and column reference
      {-|
      Extra instances for creating columns, columns references and column
      references in a wrapper directly using 'String' or 'Text'.

      Such columns will be of generic type.
      -}
    , ToCol
    , ToColRef

      -- * Generic Values
    , (/?)
    , genVal
    , genQVal
    , genQValT
    , null

      -- * SELECT
      {-|
      Additional instances which allow to construct a SELECT clause directly
      from 'String' or 'Text'.
      -}
    , SelectConstr

      -- * FROM
    , ToJoinClause

      -- * ORDER BY
    , ToSortRef

      -- * Utility functions
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Prelude hiding (null)

import qualified Data.Text as T

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor

--------------------------------------------------------------------------------
-- Table and table reference
--------------------------------------------------------------------------------

type SqlString a = String

type SqlText a = T.Text

-- | Create a table from its name provided as 'String'
instance ToTable (SqlString a) (Table a) where
    table name = Table name [] []

-- | Create a table from its name provided as 'Text'.
instance ToTable (SqlText a) (Table a) where
    table name = Table (T.unpack $ name) [] []

{-|
Convert a table reference to a table.
If the table reference is a table, then use that table.
Else, create a table using the name of this table reference.
-}
instance ToTable (TableRef a) (Table a) where
    table ref =
        case ref of
            TableRef t _ -> t
            _            -> Table (getTableRefName ref) [] []

-- | Create a table reference using its name provided as 'String'.
instance ToTableRef (SqlString a) (TableRef a) where
    tableRef name = TableRef (table name) Nothing

-- | Create a table reference using its name provided as 'Text'.
instance ToTableRef (SqlText a) (TableRef a) where
    tableRef name = TableRef (table $ T.unpack name) Nothing

--------------------------------------------------------------------------------
-- Column and column reference
--------------------------------------------------------------------------------

instance ToCol (SqlString a) (Column Undefined a) where
    toCol name = Column name Undef []

instance ToCol (SqlText a) (Column Undefined a) where
    toCol name = Column (T.unpack name) Undef []

instance ToColRef (SqlString a) (ColRef Undefined a) where
    colRef name = ColRef (ColExpr $ ColDef (toCol name) Nothing) Nothing

instance ToColRef (SqlText a) (ColRef Undefined a) where
    colRef name = ColRef (ColExpr $ ColDef columnName Nothing) Nothing
        where
            columnName = toCol $ T.unpack name

--------------------------------------------------------------------------------
-- Generic values
--------------------------------------------------------------------------------

-- | Create a placeholder "?" of undefined type for a prepared statement.
(/?) :: Value b a
(/?) = Placeholder

-- | Create a NULL value of generic type.
null :: Value b a
null = NullVal

{-|
Value of generic type which can be used unquoted in a statement.
Note: such value is used "as is". For example, if you write:
> genVal True
It's going to be parsed as follow:
> True
The above is problematic with SqLite since it accepts only 1 or 0.
So, for this database you'ld need to write:
> genVal 1

To avoid this kind of issues it's better to use the specialized constructors.
In our present case:
> boolVal True

Then the value will be parsed appropriately depending on the database vendor.
-}
genVal :: (Raw c, Show c) => c -> Value b a
genVal = GenVal

-- | Value of generic type which needs to be quoted when used in a statement.
genQVal :: String -> Value b a
genQVal = GenQVal

-- | Same as 'genQVal' but for 'Text'.
genQValT :: T.Text -> Value b a
genQValT = GenQVal . T.unpack

--------------------------------------------------------------------------------
-- SELECT
--------------------------------------------------------------------------------

type SqlString' b a = String

type SqlText' b a = T.Text

instance SelectConstr (SqlString' b a) (Query [Undefined] a) where
    select c = simpleSelect $ USelection $ ColRefWrap $ colRef c

instance SelectConstr (SqlText' b a) (Query [Undefined] a) where
    select c = simpleSelect $ USelection $ ColRefWrap $ colRef $ T.unpack c

instance SelectConstr [SqlString' b a] (Query [[Undefined]] a) where
    select cs = simpleSelect $ UsSelection $ map (ColRefWrap . colRef) cs

instance SelectConstr [SqlText' b a] (Query [[Undefined]] a) where
    select cs = simpleSelect $ UsSelection $ map toColRef cs
        where
            toColRef = ColRefWrap . colRef . T.unpack

--------------------------------------------------------------------------------
-- FROM
--------------------------------------------------------------------------------

-- | Convert an element to a list with itself as the only item.
list :: a -> [a]
list a = [a]

-- | Create an USING join clause from a string which is a column name.
instance ToJoinClause (SqlString a) (JoinClause a) where
    joinClause = JoinClauseUsing . list . ColWrap . toCol

instance ToJoinClause (SqlText a) (JoinClause a) where
    joinClause = JoinClauseUsing . list . ColWrap . toCol . T.unpack

----------------------------------------
-- ORDER BY
----------------------------------------

instance ToSortRef (SqlString a) (SortRef a) where
    sortRef name = SortRef (ColRefWrap $ colRef name) Nothing Nothing

instance ToSortRef (SqlText a) (SortRef a) where
    sortRef name = SortRef (ColRefWrap $ colRef $ T.unpack name) Nothing Nothing

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

instance ToList String [String] where
    toList x = [x]

instance ToList T.Text [T.Text] where
    toList x = [x]

instance ToList [String] [String] where
    toList = id

instance ToList [T.Text] [T.Text] where
    toList = id
