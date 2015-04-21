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
      -- * Specific constructors
      pBool
    , pNum
    , pFloat
    , pDouble
    , pInt
    , pString
    , boolVal
    , numVal
    , intVal
    , stringVal
    , nBool
    , nNum
    , nFloat
    , nDouble
    , nInt
    , nString
    
      -- * Generic constructors
    , ToSqlValues
    , value
    , values
    
      -- * Constructors for undefined types
      {-|
      Constructors producing values of undefined types.
      This allows quick & dirty coding by not having to perform operations
      on columns which are also of undefined types.
      
      Ideally, specific constructors should be rather used instead.
      -}
    , (/?)
    , null
    , undefStringVal
    , undefBoolVal
    , undefNumVal
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST

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
    
instance ToSqlValues (SqlString a) [Value String a] where
    toSqlValues a = [StringVal a]

instance ToSqlValues [SqlString a] [Value String a] where
    toSqlValues = map StringVal

instance ToSqlValues (SqlInt a) [Value Int a] where
    toSqlValues a = [IntVal a]

instance ToSqlValues [SqlInt a] [Value Int a] where
    toSqlValues = map IntVal

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

---------------------------------------
-- Specific constructors
---------------------------------------

-- | Create a placeholder "?" for a boolean value.
pBool :: Value Bool a
pBool = PlaceBool
    
-- | Create a placeholder "?" for a numeric value.
pNum :: Value Numeric a
pNum = PlaceNum
    
-- | Create a placeholder "?" for a floating number value.
pFloat :: Value Float a
pFloat = PlaceFloat

-- | Create a placeholder "?" for a double precision number value.
pDouble :: Value Double a
pDouble = PlaceDouble
    
-- | Create a placeholder "?" for a integer value.
pInt :: Value Int a
pInt = PlaceInt
    
-- | Create a placeholder ? for a string value.
pString :: Value String a
pString = PlaceString

-- | Create a boolean value.
boolVal :: Bool -> Value Bool a
boolVal = BoolVal

-- | Create a string value.
stringVal :: String -> Value String a
stringVal = StringVal

-- | Create an integer value.
intVal :: Int -> Value Int a
intVal = IntVal

-- | Create a numeric value.
numVal :: (Show b, Num b) => b -> Value Numeric a
numVal = NumericVal

-- | Create a NULL for a boolean value.
nBool :: Value Bool a
nBool = NullBool
    
-- | Create a NULL for a numeric value.
nNum :: Value Numeric a
nNum = NullNum
    
-- | Create a NULL for a floating number value.
nFloat :: Value Float a
nFloat = NullFloat

-- | Create a NULL for a double precision number value.
nDouble :: Value Double a
nDouble = NullDouble
    
-- | Create a NULL for a integer value.
nInt :: Value Int a
nInt = NullInt
    
-- | Create a NULL for a string value.
nString :: Value String a
nString = NullString

---------------------------------------
-- Generic constructors
---------------------------------------

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
-- Constructors for undefined types
---------------------------------------

-- | Create a placeholder "?" of undefined type for a prepared statement.
(/?) :: Value Undefined a
(/?) = Placeholder

-- | Create a NULL value of undefined type.
null :: Value Undefined a
null = NullVal

-- | Value of undefined type for a string.
undefStringVal :: String -> Value Undefined a
undefStringVal = UndefStringVal

-- | Value of undefined type for booleans.. 
undefBoolVal :: Bool -> Value Undefined a
undefBoolVal = UndefBoolVal

-- | Value of undefined type for numbers. 
undefNumVal :: (Show b, Num b) => b -> Value Undefined a
undefNumVal = UndefNumVal