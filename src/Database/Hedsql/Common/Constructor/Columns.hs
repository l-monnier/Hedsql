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

import Control.Lens ((&), (.~), (^.), view, set)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Coerce a given type to a list of Column.
class ToCols a b | a -> b where
    toCols :: a -> b

instance ToCols (Column a) [Column a] where
    toCols c = [c]
    
instance ToCols (SqlString a) [Column a] where
    toCols name = [Column name Nothing Nothing]
    
instance ToCols [Column a] [Column a] where
    toCols = map (head.toCols)
    
instance ToCols [SqlString a] [Column a] where
    toCols = map (head.toCols)

-- | Coerce a given type to a list of ColRef.
class ToColRefs a b | a -> b where
    toColRefs :: a -> b
    
instance ToColRefs (ColRef a) [ColRef a] where
    toColRefs ref = [ref]

instance ToColRefs (Column a) [ColRef a] where
    toColRefs a =
        [ColRef (ColExpr a Nothing) Nothing]

instance ToColRefs (Function a) [ColRef a] where
    toColRefs func = [ColRef (FuncExpr func) Nothing]

instance ToColRefs (SqlInt a) [ColRef a] where
    toColRefs int = [ColRef (ValueExpr $ SqlValueInt int) Nothing]
    
instance ToColRefs (Select a) [ColRef a] where
    toColRefs query = [ColRef (SelectExpr query) Nothing]

instance ToColRefs (SqlString a) [ColRef a] where
    toColRefs name =
        [ColRef (ColExpr (toCol name) Nothing) Nothing]
    

instance ToColRefs (SqlValue a) [ColRef a] where
    toColRefs val = [ColRef (ValueExpr val) Nothing]

instance ToColRefs [ColRef a] [ColRef a] where
    toColRefs = id
    
instance ToColRefs [Column a] [ColRef a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs [Function a] [ColRef a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs [SqlInt a] [ColRef a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs [Select a] [ColRef a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs [SqlString a] [ColRef a] where
    toColRefs = map (head.toColRefs)
    
instance ToColRefs [SqlValue a] [ColRef a] where
    toColRefs v = [ColRef (ValueExprs v) Nothing]

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Create a column reference with a qualified name.
(/.) ::
    (ToTableRefs a [TableRef c], ToColRefs b [ColRef c]) => a -> b -> ColRef c
(/.) tName cName =
    case cRef^.colRefExpr of
        ColExpr _ _ ->
            cRef &
                colRefExpr . colExprTableLabel .~ Just (tableRef tName)
        _           ->
            cRef
    where
        cRef = colRef cName

-- | Create a column reference label using AS.
as_ :: ToColRefs a [ColRef b] => a -> String -> ColRef b
as_ cRef name = set colRefLabel (Just name) (colRef cRef)

-- | Create one column which can then be used in a query or a statement.
col ::
       String        -- ^ Name of the column.
    -> SqlDataType b -- ^ Data type of the column.
    -> Column      b
col name d = toCol name & colDataType .~ Just d
    
{-|
Create a list of columns based on a list which can then be used in
a query or a statement.
-}
cols :: [(String, SqlDataType b)] -> [Column b]
cols = map (uncurry col)

-- | Create a column reference which can be used in SELECT clause.
colRef :: ToColRefs a [ColRef b] => a -> ColRef b
colRef = head.toColRefs

-- | Creates many column references which can then be used in SELECT clause.
colRefs :: ToColRefs a [ColRef b] => a -> [ColRef b]
colRefs = toColRefs

{-|
Create a SQL expression which can then be used in condition or column reference.
-}
expr :: ToColRefs a [ColRef b] => a -> Expression b
expr = head . exprs

{-|
Create SQL expressions which can then be used in condition or column references.
-}
exprs :: ToColRefs a [ColRef b] => a -> [Expression b]
exprs = map (view colRefExpr) . colRefs

-- | Convert a given type to a column.
toCol :: ToCols a [Column b] => a -> Column b
toCol = head.toCols