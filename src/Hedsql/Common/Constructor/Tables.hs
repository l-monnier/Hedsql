{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

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
    ( CoerceToTable
    , CoerceToTableRef
    , alias
    , coerceToTable
    , table
    , tableRef
    , tableRefs
    ) where

import Hedsql.Common.Constructor.Types
import Hedsql.Common.DataStructure

import Control.Lens

-- private functions.

-- | Coerce a given type to a Table.
class CoerceToTable a b | a -> b where
    coerceToTable :: a -> b

-- | Create a table from its name.
instance CoerceToTable (SqlString a) (Table a) where
    coerceToTable = Table

-- | Create a table from itself.
instance CoerceToTable (Table a) (Table a) where
    coerceToTable = id

-- | Coerce a given type to a list of TableRef.
class CoerceToTableRef a b | a -> b where
    coerceToTableRef :: a -> b

instance CoerceToTableRef (Join a) [TableRef a] where
    coerceToTableRef join = [TableJoinRef join Nothing]

instance CoerceToTableRef (SqlString a) [TableRef a] where
    coerceToTableRef name = [TableTableRef (coerceToTable name) Nothing]

instance CoerceToTableRef (Table a) [TableRef a] where
    coerceToTableRef name = [TableTableRef name Nothing]

instance CoerceToTableRef (TableRef a) [TableRef a] where
    coerceToTableRef ref = [ref]

instance CoerceToTableRef [SqlString a] [TableRef a] where
    coerceToTableRef = map (head.coerceToTableRef)
    
instance CoerceToTableRef [Table a] [TableRef a] where
    coerceToTableRef = map (head.coerceToTableRef)
    
instance CoerceToTableRef [TableRef a] [TableRef a] where
    coerceToTableRef = id

-- public functions.

-- | Create a table alias using AS.
alias :: CoerceToTableRef a [TableRef b] => a -> String -> TableRef b
alias t name =
    setAlias ref
    where
        al  = TableRefAs name []
        ref = tableRef t
        setAlias (LateralTableRef a _) = LateralTableRef a al
        setAlias (SelectTableRef  a _) = SelectTableRef  a al
        setAlias (TableJoinRef    a _) = TableJoinRef    a (Just al)
        setAlias (TableTableRef   a _) = TableTableRef   a (Just al)

-- | Create a table which can then be used as so in a query.
table ::
       CoerceToTable a (Table b)
    => a -- ^ The table itself or its name as a string.
    -> Table b
table = coerceToTable

-- | Create a table reference which can then be used in a query.
tableRef :: CoerceToTableRef a [TableRef b] => a -> TableRef b
tableRef = head.coerceToTableRef

-- | Create many table reference which can then be used in a query.
tableRefs :: CoerceToTableRef a [TableRef b] => a -> [TableRef b]
tableRefs = coerceToTableRef

-- | Create many tables which can then be used as so in a query.
tables ::
       CoerceToTable a (Table b)
    => [a] -- ^ The tables themselves or their name as a string.
    -> [Table b]
tables = map table