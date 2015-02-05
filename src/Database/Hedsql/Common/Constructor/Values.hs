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
    ( ToSqlValues
    , (/?)
    , value
    , values
    ) where

import Database.Hedsql.Common.DataStructure
import Database.Hedsql.Common.Constructor.Types

-- private functions.

-- | Coerce a given type to a list of SqlValue.
class ToSqlValues a b | a -> b where
    toSqlValues :: a -> b

instance ToSqlValues  (SqlInt a) [SqlValue a] where
    toSqlValues a = [SqlValueInt a]

instance ToSqlValues  [SqlInt a] [SqlValue a] where
    toSqlValues = map SqlValueInt

{-|
If the value is "Just a", then "coerce" will be applied on "a".
Else, the value is "Nothing" and it will be considered as a NULL value.
-}
instance ToSqlValues a [SqlValue a]
      => ToSqlValues (Maybe a) [SqlValue a] where
    toSqlValues (Just a) = toSqlValues a
    toSqlValues Nothing = [SqlValueNull]

instance ToSqlValues (SqlValue a) [SqlValue a] where
    toSqlValues a = [id a]

instance ToSqlValues [SqlValue a] [SqlValue a] where
    toSqlValues = id
    
instance ToSqlValues (SqlString a) [SqlValue a] where
    toSqlValues a = [SqlValueString a]

instance ToSqlValues [SqlString a] [SqlValue a] where
    toSqlValues = map SqlValueString

-- public functions.

-- | Create a placeholder "?" for a prepared statement.
(/?) :: SqlValue a
(/?) = Placeholder

{-|
Convert a primitive value so it can be used in SQL queries as "raw" values.
-}
value :: ToSqlValues a [SqlValue b] => a -> SqlValue b
value = head.toSqlValues
    
{-|
Convert a list of primitive values so they can be used in SQL queries
as "raw" values.
-}
values :: ToSqlValues a [SqlValue b] => a -> [SqlValue b]
values = toSqlValues