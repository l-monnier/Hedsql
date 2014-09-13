{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-|
Module      : Hedsql/Common/Constructor/Columns.hs
Description : Constructor functions for columns.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for columns references which can then be used in queries.
-}
module Hedsql.Common.Constructor.Columns
    (
      ColumnConstructor
    , ColRefConstruct
    , (/.)
    , column
    , columns
    , out
    , toColRef
    , toColRefs
    , toExpr
    , toExprs
    ) where

import Hedsql.Common.Constructor.Tables
import Hedsql.Common.Constructor.Values
import Hedsql.Common.DataStructure.Base

import Control.Lens

-- private functions.

-- public functions.
   
-- | Create a column which can then be used in a query.
class ColumnConstructor a where
    
    -- | Create one column based on a value.
    column :: a -> Column
    
    -- | Create a list of columns based on a list of values.
    columns :: a -> [Column]

instance ColumnConstructor Char where
    column name = column [name]
    columns a = [column a]

instance ColumnConstructor Column where
    column a = a
    columns a = [a]
    
instance ColumnConstructor [Column] where
    column = head
    columns a = a
    
instance ColumnConstructor String where
    column name = Column name Nothing Nothing Nothing
    columns = map column
    
instance ColumnConstructor [String] where
    column = column.head
    columns = map column

-- | Create a column reference which can be used in SELECT clause.
class ColRefConstruct a where
    toColRef :: a -> ColRef
    toColRefs :: a -> [ColRef]

instance ColRefConstruct ColRef where
    toColRef a = a
    toColRefs a = [a]

instance ColRefConstruct [ColRef] where
    toColRef = head
    toColRefs a = a

instance ColRefConstruct Column where
    toColRef a = ColRef (ColExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [Column] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct CurrentDate where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct Function where
    toColRef a = ColRef (FuncExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct Int where
    toColRef a = ColRef (ValueExpr (toValue a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [Int] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct Joker where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct Operator where
    toColRef a = ColRef (OperatorExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [Operator] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct Random where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]    
    
instance ColRefConstruct SelectQuery where
    toColRef a = ColRef (SelectExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [SelectQuery] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct String where
    toColRef a = ColRef (ColExpr (column a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [String] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct SqlValue where
    toColRef a = ColRef (ValueExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [SqlValue] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct Sum where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]
    
-- | Create a column with a qualified name.
(/.) :: (TableConstructor a, ColumnConstructor b) => a -> b -> Column
(/.) tName cName = set colTable (Just (table tName)) $ column cName

{-|
Create a SQL expression which can then be used in condition or column reference.
Use the ColRef class to avoid rewritting each instance.
-}
toExpr :: ColRefConstruct a => a -> Expression
toExpr a = toColRef a ^. colRefExpr

{-|
Create SQL expressions which can then be used in condition or column references.
-}
toExprs :: ColRefConstruct a => [a] -> [Expression]
toExprs = map toExpr

-- | Create column reference label using AS.
out :: ColRef -> Label -> ColRef
out colRef name = set colRefLabel (Just name) colRef