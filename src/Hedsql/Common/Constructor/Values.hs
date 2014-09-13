{-# LANGUAGE FlexibleInstances #-}

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
      SqlValueConstructor
    , toValue
    , toValues
    ) where

import Hedsql.Common.DataStructure.Base

-- private functions.

-- public functions.

{-|
Convert primitive values so they can be used in SQL queries as "raw" values.
-}
class SqlValueConstructor a where
    
    -- | Convert one value.
    toValue :: a -> SqlValue
    
    -- | Convert a list of values.
    toValues :: a -> [SqlValue]
    
instance SqlValueConstructor Int where
    toValue = SqlValueInt
    toValues a = [toValue a]

instance SqlValueConstructor [Int] where
    toValue = toValue.head
    toValues = map toValue

{-|
If the value is "Just a", then "toValue" will be applied on "a".
Else, the value is "Nothing" and it will be considered as a NULL value.
-}
instance SqlValueConstructor a => SqlValueConstructor (Maybe a) where
    toValue (Just a) = toValue a
    toValue Nothing = SqlValueNull
    toValues (Just a) = toValues a
    toValues Nothing = [SqlValueNull]

instance SqlValueConstructor SqlValue where
    toValue a = a
    toValues a = [a]

instance SqlValueConstructor [SqlValue] where
    toValue = head
    toValues a = a
    
instance SqlValueConstructor String where
    toValue = SqlValueString
    toValues a = [toValue a]

instance SqlValueConstructor [String] where
    toValue = toValue.head
    toValues = map toValue