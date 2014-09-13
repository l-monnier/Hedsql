{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Hedsql/Common/Constructor/Tables.hs
Description : Constructor functions for tables.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for table references which can then be used in queries.
-}
module Hedsql.Common.Constructor.Tables
    (
      AliasConstruct
    , TableConstructor
    , TableRefConstruct
    , alias
    , table
    , toTableRef
    , toTableRefs
    ) where

import Hedsql.Common.DataStructure.Base

import Control.Lens

-- private functions.

-- public functions.

-- | Create table aliases using AS.
class AliasConstruct a where
    alias :: a -> String -> a

instance AliasConstruct Table where
    alias t name = set tableAlias (Just (TableReferenceAlias name [])) t

-- | Create a table reference which can then be used in a query.
class TableConstructor a where
   table :: a -> Table

instance TableConstructor String where
   table name = Table name Nothing

instance TableConstructor Table where
   table a = a
   
-- | Create table references.
class TableRefConstruct a where
    toTableRef :: a -> TableReference
    toTableRefs :: a -> [TableReference]

instance TableRefConstruct String where
    toTableRef = toTableRef.table
    toTableRefs = toTableRefs.table

instance TableRefConstruct [String] where
    toTableRef = toTableRef.table.head
    toTableRefs = toTableRefs.map table

instance TableRefConstruct Table where
    toTableRef a = TableTableReference a
    toTableRefs a = [toTableRef a]

instance TableRefConstruct [Table] where
    toTableRef = toTableRef.head
    toTableRefs = map toTableRef
    
instance TableRefConstruct TableReference where
    toTableRef a = a
    toTableRefs a = [a]

instance TableRefConstruct [TableReference] where
    toTableRef = head
    toTableRefs a = a