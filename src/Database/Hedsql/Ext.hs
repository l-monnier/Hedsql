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
the typDatabase.Hedsql.Columnsty of Hedsql to perform
static type checks.
However, this isn't a black or white choice as you can mix both styles.

= Example

The below example use directly a 'String' in the 'select' clause and neither
age nor the 'Int' value have a proper type: age is generic as the value.
> select "firstName" /++ from "People" /++ where_ ("age" /> genVal (18::Int))

= Deactivate the GHC OverloadedStrings language extension

If you wish to provide parameters as simple 'String', if activated,
you should turn off the GHC OverloadedStrings language extension.

> LANGUAGE NoOverloadedStrings

This will allow to write queries like this:

> select "col1" /++ from "table1"

instead of:

> select ("col1"::String) /++ from ("table1"::String)

For more explanations related to the why and how, please refer to the
following discussion:
<http://haskell.1045720.n5.nabble.com/Proposal-Improving-the-IsString-String
-instance-td5734824.html>
-}
module Database.Hedsql.Ext
    (
      -- * Table and table reference
      {-|
      Extra instances provinding shortcuts such the ability to convert a
      table reference back to a table or creating tables directly from 'String'.
      -}
      ToTables
    , ToTableRefs
    
      -- * Column and column reference
      {-|
      Extra instances for creating columns, columns references and column
      references in a wrapper directly using 'String'.
      
      Such columns will be of generic type.
      -}
    , ToCols
    , ToColRefs
    , ToColRefWraps
    
      -- * Generic Values
    , (/?)
    , genVal
    , genQVal
    , null
    
      -- * SELECT
      {-|
      Additional instances which allow to construct a SELECT clause directly
      from 'String'.
      -}
    , SelectConstr
    
      -- * FROM
    , ToJoinClauses
    
      -- * ORDER BY
    , ToSortRefs
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor

import Prelude hiding (null)

--------------------------------------------------------------------------------
-- Table and table reference
--------------------------------------------------------------------------------

-- | Create a table from its name.
instance ToTables (SqlString a) [Table a] where
    toTables name = [Table name [] []]
    
{-|
Convert a table reference to a list of tables.
If the table reference is a table, then use that table.
Else, create a table using the name of this table reference.
-}
instance ToTables (TableRef a) [Table a] where
    toTables ref =
        case ref of
            TableRef t _ -> [t]
            _            -> [Table (getTableRefName ref) [] []]

-- | Create a list containing one table reference using its name.            
instance ToTableRefs (SqlString a) [TableRef a] where
    toTablesRef name = [TableRef (table name) Nothing]

-- | Create a list of table references using the provided names.
instance ToTableRefs [SqlString a] [TableRef a] where
    toTablesRef = map (head.toTablesRef)

--------------------------------------------------------------------------------
-- Column and column reference
--------------------------------------------------------------------------------

-- TODO: replace it by a generic type rather than an undefined one.

instance ToCols (SqlString a) [Column Undefined a] where
    toCols name = [Column name Undef []]
    
instance ToCols [SqlString a] [Column Undefined a] where
    toCols = map (head.toCols)
    
instance ToColRefs (SqlString a) [ColRef Undefined a] where
    toColRefs name = [ColRef (ColExpr $ ColDef (toCol name) Nothing) Nothing]

instance ToColRefs [SqlString a] [ColRef Undefined a] where
    toColRefs = map (head.toColRefs)
    
instance ToColRefWraps (SqlString a) [ColRefWrap a] where
    toColRefWraps = mkColRefWraps

instance ToColRefWraps [SqlString a] [ColRefWrap a] where
    toColRefWraps = mkColRefWrap

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

--------------------------------------------------------------------------------
-- SELECT
--------------------------------------------------------------------------------

type SqlString a = String
type SqlString' b a = String

instance SelectConstr (SqlString' b a) (Select [Undefined] a) where
    select c = simpleSelect $ USelection $ ColRefWrap $ colRef c

instance SelectConstr [SqlString' b a] (Select [[Undefined]] a) where
    select c = simpleSelect $ UsSelection $ map (ColRefWrap . colRef) c

--------------------------------------------------------------------------------
-- FROM
--------------------------------------------------------------------------------

-- | Convert an element to a list with itself as the only item.
list :: a -> [a]
list a = [a]

-- | Create an USING join clause from a string which is a column name.
instance ToJoinClauses (SqlString a) [JoinClause a] where
    toJoinClauses = list . JoinClauseUsing . map ColWrap . toCols

-- | Create an USING join clause from a list of strings which are column names.    
instance ToJoinClauses [SqlString a] [JoinClause a] where
    toJoinClauses = list . JoinClauseUsing . map ColWrap . toCols
    
----------------------------------------
-- ORDER BY
----------------------------------------    

instance ToSortRefs (SqlString a) [SortRef a] where
    toSortRefs name = [SortRef (ColRefWrap $ colRef name) Nothing Nothing]

instance ToSortRefs [SqlString a] [SortRef a] where
    toSortRefs =
        map (\name -> SortRef (ColRefWrap $ colRef name) Nothing Nothing)