{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Common/Constructor/Values.hs
Description : Constructor functions for values.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for values which can then be used in queries.
-}
module Database.Hedsql.Common.Constructor.Values
    ( CoerceToSqlValue
    , (/?)
    , value
    , values
    ) where

import Database.Hedsql.Common.DataStructure
import Database.Hedsql.Common.Constructor.Types

-- private functions.

-- | Coerce a given type to a list of SqlValue.
class CoerceToSqlValue a b | a -> b where
    coerceToSqlValue :: a -> b

instance CoerceToSqlValue  (SqlInt a) [SqlValue a] where
    coerceToSqlValue a = [SqlValueInt a]

instance CoerceToSqlValue  [SqlInt a] [SqlValue a] where
    coerceToSqlValue = map SqlValueInt

{-|
If the value is "Just a", then "coerce" will be applied on "a".
Else, the value is "Nothing" and it will be considered as a NULL value.
-}
instance CoerceToSqlValue a [SqlValue a]
      => CoerceToSqlValue (Maybe a) [SqlValue a] where
    coerceToSqlValue (Just a) = coerceToSqlValue a
    coerceToSqlValue Nothing = [SqlValueNull]

instance CoerceToSqlValue (SqlValue a) [SqlValue a] where
    coerceToSqlValue a = [id a]

instance CoerceToSqlValue [SqlValue a] [SqlValue a] where
    coerceToSqlValue = id
    
instance CoerceToSqlValue (SqlString a) [SqlValue a] where
    coerceToSqlValue a = [SqlValueString a]

instance CoerceToSqlValue [SqlString a] [SqlValue a] where
    coerceToSqlValue = map SqlValueString

-- public functions.

-- | Create a placeholder "?" for a prepared statement.
(/?) :: SqlValue a
(/?) = Placeholder

{-|
Convert a primitive value so it can be used in SQL queries as "raw" values.
-}
value :: CoerceToSqlValue a [SqlValue b] => a -> SqlValue b
value = head.coerceToSqlValue
    
{-|
Convert a list of primitive values so they can be used in SQL queries
as "raw" values.
-}
values :: CoerceToSqlValue a [SqlValue b] => a -> [SqlValue b]
values = coerceToSqlValue