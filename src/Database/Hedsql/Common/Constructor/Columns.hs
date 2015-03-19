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
    , ToColRefWraps
    , (/.)
    , as_
    , col
    , cols
    , toCol
    , toCols
    , colRef
    , colRefs
    , colRefWrap
    , colRefWraps
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

instance ToColRefs [ColRef b a] [ColRef b a] where
    toColRefs = id

instance ToColRefs (Column b a) [ColRef b a] where
    toColRefs a =
        [ColRef (ColExpr $ ColDef a Nothing) Nothing]
    
instance ToColRefs [Column b a] [ColRef b a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs (Expression b a) [ColRef b a] where
    toColRefs e = [ColRef e Nothing]

instance ToColRefs [Expression b a] [ColRef b a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs (Value b a) [ColRef b a] where
    toColRefs val = [ColRef (Value val) Nothing]

instance ToColRefs [Value b a] [ColRef [b] a] where
    toColRefs vals = [ColRef (Values vals) Nothing]

instance ToColRefs (SqlString a) [ColRef Undefined a] where
    toColRefs name = [ColRef (ColExpr $ ColDef (toCol name) Nothing) Nothing]

instance ToColRefs [SqlString a] [ColRef Undefined a] where
    toColRefs = map (head.toColRefs)

instance ToColRefs (Select b a) [ColRef b a] where
    toColRefs query = [ColRef (SelectExpr query) Nothing]

instance ToColRefs [Select b a] [ColRef b a] where
    toColRefs = map (head.toColRefs)

-- | Coerce a type to a list of ColRefWrap.
class ToColRefWraps a b | a -> b where
    toColRefWraps :: a -> b

instance ToColRefWraps (ColRef b a) [ColRefWrap a] where
    toColRefWraps = mkColRefWraps

instance ToColRefWraps [ColRef b a] [ColRefWrap a] where
    toColRefWraps = mkColRefWrap

instance ToColRefWraps (Column b a) [ColRefWrap a] where
    toColRefWraps = mkColRefWraps

instance ToColRefWraps [Column b a] [ColRefWrap a] where
    toColRefWraps = mkColRefWrap

instance ToColRefWraps (Expression b a) [ColRefWrap a] where
    toColRefWraps = mkColRefWraps

instance ToColRefWraps [Expression b a] [ColRefWrap a] where
    toColRefWraps = mkColRefWrap

instance ToColRefWraps (Value b a) [ColRefWrap a] where
    toColRefWraps = mkColRefWraps

instance ToColRefWraps [Value b a] [ColRefWrap a] where
    toColRefWraps = mkColRefWrap

instance ToColRefWraps (SqlString a) [ColRefWrap a] where
    toColRefWraps = mkColRefWraps

instance ToColRefWraps [SqlString a] [ColRefWrap a] where
    toColRefWraps = mkColRefWrap

instance ToColRefWraps (Select b a) [ColRefWrap a] where
    toColRefWraps = mkColRefWraps

instance ToColRefWraps [Select b a] [ColRefWrap a] where
    toColRefWraps = mkColRefWrap

instance ToColRefWraps (ColRefWrap a) [ColRefWrap a] where
    toColRefWraps a = [a]

instance ToColRefWraps [ColRefWrap a] [ColRefWrap a] where
    toColRefWraps = id

mkColRefWrap :: ToColRefs a [ColRef c b] => a -> [ColRefWrap b]
mkColRefWrap = map ColRefWrap . colRefs

mkColRefWraps :: ToColRefs a [ColRef c b] => a -> [ColRefWrap b]
mkColRefWraps c = [ColRefWrap $ colRef c]

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

-- | Create a column reference wrapper for heteregeneous lists.
colRefWrap :: ToColRefWraps a [ColRefWrap b] => a -> ColRefWrap b
colRefWrap = head . toColRefWraps

-- | Creates many column references for heteregeneous lists.
colRefWraps :: ToColRefWraps a [ColRefWrap b] => a -> [ColRefWrap b]
colRefWraps = toColRefWraps

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