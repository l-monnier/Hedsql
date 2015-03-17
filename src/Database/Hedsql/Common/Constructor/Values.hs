{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
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
    ( 
      -- * Generic constructors
      ToSqlValues
    , (/?)
    , value
    , values
    
      -- * Specific constructors
    , intVal
    , null
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.DataStructure

import Prelude hiding (null)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Types used for the coercion.
type SqlBool   a = Bool
type SqlInt    a = Int
type SqlString a = String

-- | Coerce a given type to a list of SqlValue.
class ToSqlValues a b | a -> b where
    toSqlValues :: a -> b

instance ToSqlValues (Value a b) [Value a b] where
    toSqlValues a = [a]

instance ToSqlValues [Value a b] [Value a b] where
    toSqlValues = id

instance ToSqlValues (SqlBool a) [Value Bool a] where
    toSqlValues a = [BoolVal a]

instance ToSqlValues [SqlBool a] [Value Bool a] where
    toSqlValues = map BoolVal
    
instance ToSqlValues (SqlString a) [Value Text a] where
    toSqlValues a = [StringVal a]

instance ToSqlValues [SqlString a] [Value Text a] where
    toSqlValues = map StringVal

instance ToSqlValues (SqlInt a) [Value Numeric a] where
    toSqlValues a = [IntVal a]

instance ToSqlValues [SqlInt a] [Value Numeric a] where
    toSqlValues = map IntVal

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

---------------------------------------
-- Generic constructors
---------------------------------------

-- | Create a placeholder "?" for a prepared statement.
(/?) :: Value b a
(/?) = Placeholder

{-|
Convert a primitive value so it can be used in SQL queries as "raw" values.
-}
value :: ToSqlValues a [Value b c] => a -> Value b c
value = head.toSqlValues
    
{-|
Convert a list of primitive values so they can be used in SQL queries
as "raw" values.
-}
values :: ToSqlValues a [Value b c] => a -> [Value b c]
values = toSqlValues

---------------------------------------
-- Specific constructors
---------------------------------------

-- | Create an integer value.
intVal :: Int -> Value Numeric a
intVal = IntVal

-- | Create a NULL value.
null :: Value Undefined a
null = NullVal