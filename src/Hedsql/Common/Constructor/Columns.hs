{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

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
    ( CoerceToCol
    , CoerceToColRef
    , (/.)
    , as_
    , column
    , columns
    , colRef
    , colRefs
    , expr
    , exprs
    ) where

import Hedsql.Common.Constructor.Tables
import Hedsql.Common.Constructor.Types
import Hedsql.Common.Constructor.Values
import Hedsql.Common.DataStructure

import Control.Lens ((^.), view, set)

-- private functions.

-- | Coerce a given type to a list of Column.
class CoerceToCol a b | a -> b where
    coerceToCol :: a -> b

instance CoerceToCol (Column a) [Column a] where
    coerceToCol col = [col]
    
instance CoerceToCol (SqlString a) [Column a] where
    coerceToCol name = [Column name Nothing Nothing Nothing]
    
instance CoerceToCol [Column a] [Column a] where
    coerceToCol = map (head.coerceToCol)
    
instance CoerceToCol [SqlString a] [Column a] where
    coerceToCol = map (head.coerceToCol)

-- | Coerce a given type to a list of ColRef.
class CoerceToColRef a b | a -> b where
    coerceToColRef :: a -> b
    
instance CoerceToColRef (ColRef a) [ColRef a] where
    coerceToColRef ref = [ref]

instance CoerceToColRef (Column a) [ColRef a] where
    coerceToColRef a = [ColRef (ColExpr a) Nothing]

instance CoerceToColRef (Function a) [ColRef a] where
    coerceToColRef func = [ColRef (FuncExpr func) Nothing]

instance CoerceToColRef (SqlInt a) [ColRef a] where
    coerceToColRef int = [ColRef (ValueExpr $ SqlValueInt int) Nothing]
    
instance CoerceToColRef (Select a) [ColRef a] where
    coerceToColRef query = [ColRef (SelectExpr query) Nothing]

instance CoerceToColRef (SqlString a) [ColRef a] where
    coerceToColRef name = [ColRef (ColExpr (column name)) Nothing]

instance CoerceToColRef (SqlValue a) [ColRef a] where
    coerceToColRef val = [ColRef (ValueExpr val) Nothing]

instance CoerceToColRef [ColRef a] [ColRef a] where
    coerceToColRef = id
    
instance CoerceToColRef [Column a] [ColRef a] where
    coerceToColRef = map (head.coerceToColRef)

instance CoerceToColRef [Function a] [ColRef a] where
    coerceToColRef = map (head.coerceToColRef)

instance CoerceToColRef [SqlInt a] [ColRef a] where
    coerceToColRef = map (head.coerceToColRef)

instance CoerceToColRef [Select a] [ColRef a] where
    coerceToColRef = map (head.coerceToColRef)

instance CoerceToColRef [(SqlString a)] [ColRef a] where
    coerceToColRef = map (head.coerceToColRef)
    
instance CoerceToColRef [SqlValue a] [ColRef a] where
    coerceToColRef = map (head.coerceToColRef)

-- public functions.

-- | Create a column reference with a qualified name.
(/.) ::
    (CoerceToTable a (Table c), CoerceToCol b [Column c]) => a -> b -> ColRef c
(/.) tName cName = 
    ColRef (ColExpr col) Nothing
    where
        col = set colTable (Just (table tName)) $ column cName

-- | Create column reference label using AS.
as_ :: ColRef a -> Label -> ColRef a
as_ colRef name = set colRefLabel (Just name) colRef

-- | Create one column based on a value which can then be used in a query.
column :: CoerceToCol a [Column b] => a -> Column b
column = head.coerceToCol
    
{-|
Create a list of columns based on a list of values which can then be used in
a query.
-}
columns :: CoerceToCol a [Column b] => a -> [Column b]
columns = coerceToCol

-- | Create a column reference which can be used in SELECT clause.
colRef :: CoerceToColRef a [ColRef b] => a -> ColRef b
colRef = head.coerceToColRef

-- | Creates many column references which can then be used in SELECT clause.
colRefs :: CoerceToColRef a [ColRef b] => a -> [ColRef b]
colRefs = coerceToColRef

{-|
Create a SQL expression which can then be used in condition or column reference.
-}
expr :: CoerceToColRef a [ColRef b] => a -> Expression b
expr = head . exprs

{-|
Create SQL expressions which can then be used in condition or column references.
-}
exprs :: CoerceToColRef a [ColRef b] => a -> [Expression b]
exprs = map (view colRefExpr) . colRefs