{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Common/Constructor/Columns.hs
Description : Constructor functions for columns.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for columns references which can then be used in queries.
-}
module Database.Hedsql.Common.Constructor.Columns
    ( ToCols
    , ToColRefs
    , (/.)
    , as_
    , col
    , cols
    , toCol
    , toCols
    , colRef
    , colRefs
    , expr
    , exprs
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.Constructor.Tables
import Database.Hedsql.Common.Constructor.Types
import Database.Hedsql.Common.Constructor.Values()
import Database.Hedsql.Common.DataStructure

import Control.Lens ((^.), view, set)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Coerce a given type to a list of Column.
class ToCols a b | a -> b where
    toCols :: a -> b

instance ToCols (Column a b) [Column a b] where
    toCols c = [c]
    
instance ToCols (SqlString a) [Column Undefined a] where
    toCols name = [Column name Undef []]
    
instance ToCols [Column a b] [Column a b] where
    toCols = map (head.toCols)
    
instance ToCols [SqlString a] [Column Undefined a] where
    toCols = map (head.toCols)

-- | Coerce a given type to a list of ColRef.
class ToColRefs a b | a -> b where
    toColRefs :: a -> b
    
instance ToColRefs (ColRef b a) [ColRef b a] where
    toColRefs ref = [ref]

instance ToColRefs (Column b a) [ColRef b a] where
    toColRefs a =
        [ColRef (ColExpr $ ColDef a Nothing) Nothing]

instance ToColRefs (Expression b a) [ColRef b a] where
    toColRefs e = [ColRef e Nothing]

instance ToColRefs (Value b a) [ColRef b a] where
    toColRefs val = [ColRef (Value val) Nothing]

instance ToColRefs (SqlString a) [ColRef Undefined a] where
    toColRefs name = [ColRef (ColExpr $ ColDef (toCol name) Nothing) Nothing]

instance ToColRefs (Select b a) [ColRef b a] where
    toColRefs query = [ColRef (SelectExpr query) Nothing]

instance ToColRefs [ColRef b a] [ColRef b a] where
    toColRefs = id
    
instance ToColRefs [Column b a] [ColRef b a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs [Expression b a] [ColRef b a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs [Value b a] [ColRef b a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs [Select b a] [ColRef b a] where
    toColRefs = map (head.toColRefs)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Create a column reference with a qualified name.
(/.) ::
    (  ToTableRefs a [TableRef c]
    ,  ToColRefs   b [ColRef d c]
    )
    => a
    -> b
    -> ColRef d c
(/.) tName cName =
    case cRef^.colRefExpr of
        ColExpr colDef ->
            set colRefExpr (ColExpr cDef) cRef
            where
                cDef = set colExprTableLabel (Just $ tableRef tName) colDef
        _           ->
            cRef
    where
        cRef = colRef cName

-- | Create a column reference label using AS.
as_ :: ToColRefs a [ColRef b c] => a -> String -> ColRef b c
as_ cRef name = set colRefLabel (Just name) (colRef cRef)

-- | Create one column which can then be used in a query or a statement.
col ::
       String       -- ^ Name of the column.
    -> DataType b a -- ^ Data type of the column.
    -> Column   b a
col name d = Column name d []
    
{-|
Create a list of columns based on a list which can then be used in
a query or a statement.
-}
cols :: [(String, DataType b a)] -> [Column b a]
cols = map (uncurry col)

-- | Create a column reference which can be used in SELECT clause.
colRef :: ToColRefs a [ColRef b c] => a -> ColRef b c
colRef = head.toColRefs

-- | Creates many column references which can then be used in SELECT clause.
colRefs :: ToColRefs a [ColRef b c] => a -> [ColRef b c]
colRefs = toColRefs

{-|
Create a SQL expression which can then be used in condition or column reference.
-}
expr :: ToColRefs a [ColRef b c] => a -> Expression b c
expr = head . exprs

{-|
Create SQL expressions which can then be used in condition or column references.
-}
exprs :: ToColRefs a [ColRef b c] => a -> [Expression b c]
exprs = map (view colRefExpr) . colRefs

-- | Convert a given type to a column.
toCol :: ToCols a [Column b c] => a -> Column b c
toCol = head.toCols