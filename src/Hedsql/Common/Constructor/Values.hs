{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Hedsql/Common/Constructor/Values.hs
Description : Constructor functions for values.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for values which can then be used in queries.
-}
module Hedsql.Common.Constructor.Values
    (
      value
    , values
    ) where

import Hedsql.Common.DataStructure.Base
import Hedsql.Helpers.Coerce

-- private functions.

instance Coerce Int (SqlValue a) where
    coerce = SqlValueInt

instance Coerce [Int] [SqlValue a] where
    coerce = map coerce

{-|
If the value is "Just a", then "coerce" will be applied on "a".
Else, the value is "Nothing" and it will be considered as a NULL value.
-}
instance Coerce a (SqlValue a) => Coerce (Maybe a) (SqlValue a) where
    coerce (Just a) = coerce a
    coerce Nothing = SqlValueNull

instance Coerce (SqlValue a) (SqlValue a) where
    coerce = id

instance Coerce [SqlValue a] [SqlValue a] where
    coerce = id
    
instance Coerce String (SqlValue a) where
    coerce = SqlValueString

instance Coerce [String] [SqlValue a] where
    coerce = map coerce

-- public functions.

{-|
Convert a primitive value so it can be used in SQL queries as "raw" values.
-}
value :: Coerce a (SqlValue a) => a -> SqlValue a
value = coerce
    
{-|
Convert a list of primitive values so they can be used in SQL queries
as "raw" values.
-}
values :: Coerce [a] [SqlValue a] => [a] -> [SqlValue a]
values = coerce