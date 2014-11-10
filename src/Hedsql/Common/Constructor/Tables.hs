{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , coerce
    , table
    ) where

import Hedsql.Common.DataStructure.Base

import Control.Lens hiding (coerce)

-- public functions.

-- | Create a table alias using AS.
alias :: Table a -> String -> Table a
alias t name = set tableAlias (Just (TableRefAs name [])) t

-- | Create a table which can then be used as so in a query.
table
    :: Coerce a (Table b)
    => a -- ^ The table itself or its name as a string.
    -> (Table b)
table a = coerce a

-- | Create a table from its name.
instance Coerce String (Table a) where
    coerce name = Table name Nothing

-- | Create a table from itself.
instance Coerce (Table a) (Table a) where
    coerce = id

-- | Convert a type to another type. 
class Coerce a b where
    coerce :: a -> b

instance Coerce String (TableRef a) where
    coerce name = TableTableRef $ coerce name

instance Coerce (Table a) (TableRef a) where
    coerce = TableTableRef

instance Coerce (TableRef a) (TableRef a) where
    coerce = id

instance Coerce [TableRef a] (TableRef a) where
    coerce = head

instance Coerce (TableRef a) [TableRef a] where
    coerce a = [a]

instance Coerce [String] [TableRef a] where
    coerce =  map coerce

instance Coerce [Table a] [TableRef a] where
    coerce = map coerce

instance Coerce [TableRef a] [TableRef a] where
    coerce = id