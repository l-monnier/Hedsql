{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeSynonymInstances  #-}

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
      alias
    , table
    , tableRef
    , tableRefs
    ) where

import Hedsql.Common.DataStructure.Base
import Hedsql.Helpers.Coerce

import Control.Lens hiding (coerce)

-- private functions.

-- | Create a table from its name.
instance Coerce String (Table a) where
    coerce = Table

-- | Create a table from itself.
instance Coerce (Table a) (Table a) where
    coerce = id

instance Coerce String [TableRef a] where
    coerce name = [TableTableRef (coerce name) Nothing]

instance Coerce (Table a) [TableRef a] where
    coerce name = [TableTableRef name Nothing]

instance Coerce (TableRef a) [TableRef a] where
    coerce ref = [ref]

instance Coerce [String] [TableRef a] where
    coerce = map (head.coerce)
    
instance Coerce [Table a] [TableRef a] where
    coerce = map (head.coerce)
    
instance Coerce [TableRef a] [TableRef a] where
    coerce = id

-- public functions.

-- | Create a table alias using AS.
alias :: Coerce a [TableRef a] => a -> String -> TableRef a
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
table
    :: Coerce a (Table b)
    => a -- ^ The table itself or its name as a string.
    -> Table b
table = coerce

-- | Create a table reference which can then be used in a query.
tableRef :: Coerce a [TableRef a] => a -> TableRef a
tableRef = head.coerce

-- | Create many table reference which can then be used in a query.
tableRefs :: Coerce a [TableRef a] => a -> [TableRef a]
tableRefs = coerce

-- | Create many tables which can then be used as so in a query.
tables
    :: Coerce a (Table b)
    => [a] -- ^ The tables themselves or their name as a string.
    -> [Table b]
tables = map table