{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
      (/.)
    , as_
    , column
    , columns
    , colRef
    , colRefs
    , expr
    , exprs
    ) where

import Hedsql.Common.Constructor.Tables
import Hedsql.Common.Constructor.Values
import Hedsql.Common.DataStructure.Base
import Hedsql.Helpers.Coerce

import Control.Lens ((^.), set)

-- private functions.

instance Coerce (Column a) [Column a] where
    coerce col = [col]
    
instance Coerce String [Column a] where
    coerce name = [Column name Nothing Nothing Nothing]
    
instance Coerce [Column a] [Column a] where
    coerce = map (head.coerce)
    
instance Coerce [String] [Column a] where
    coerce = map (head.coerce)
    
instance Coerce (ColRef a) [ColRef a] where
    coerce ref = [ref]

instance Coerce (Column a) [ColRef a] where
    coerce a = [ColRef (ColExpr a) Nothing]

instance Coerce (Function a) [ColRef a] where
    coerce func = [ColRef (FuncExpr func) Nothing]

instance Coerce Int [ColRef a] where
    coerce int = [ColRef (ValueExpr $ SqlValueInt int) Nothing]
    
instance Coerce (Select a) [ColRef a] where
    coerce query = [ColRef (SelectExpr query) Nothing]

instance Coerce String [ColRef a] where
    coerce name = [ColRef (ColExpr (column name)) Nothing]

instance Coerce (SqlValue a) [ColRef a] where
    coerce val = [ColRef (ValueExpr val) Nothing]

instance Coerce [ColRef a] [ColRef a] where
    coerce = id
    
instance Coerce [Column a] [ColRef a] where
    coerce = map (head.coerce)

instance Coerce [Function a] [ColRef a] where
    coerce = map (head.coerce)

instance Coerce [Int] [ColRef a] where
    coerce = map (head.coerce)

instance Coerce [Select a] [ColRef a] where
    coerce = map (head.coerce)

instance Coerce [String] [ColRef a] where
    coerce = map (head.coerce)
    
instance Coerce [SqlValue a] [ColRef a] where
    coerce = map (head.coerce)

-- public functions.

-- | Create a column reference with a qualified name.
(/.) :: (Coerce a (Table c), Coerce b [Column c]) => a -> b -> ColRef c
(/.) tName cName = 
    ColRef (ColExpr col) Nothing
    where
        col = set colTable (Just (table tName)) $ column cName

-- | Create column reference label using AS.
as_ :: ColRef a -> Label -> ColRef a
as_ colRef name = set colRefLabel (Just name) colRef

-- | Create one column based on a value which can then be used in a query.
column :: Coerce a [Column b] => a -> Column b
column = head.coerce
    
{-|
Create a list of columns based on a list of values which can then be used in
a query.
-}
columns :: Coerce a [Column b] => a -> [Column b]
columns = coerce

-- | Create a column reference which can be used in SELECT clause.
colRef :: Coerce a [ColRef b] => a -> ColRef b
colRef = head.coerce

-- | Creates many column references which can then be used in SELECT clause.
colRefs :: Coerce a [ColRef b] => a -> [ColRef b]
colRefs = coerce

{-|
Create a SQL expression which can then be used in condition or column reference.
-}
expr :: Coerce a [ColRef a] => a -> Expression a
expr a = head (coerce a) ^. colRefExpr

{-|
Create SQL expressions which can then be used in condition or column references.
-}
exprs :: Coerce a [ColRef a] => [a] -> [Expression a]
exprs = map expr