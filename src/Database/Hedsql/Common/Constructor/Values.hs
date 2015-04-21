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
    , genVal
    , genQVal
    , null
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
(/?) :: Value b a
(/?) = Placeholder

-- | Create a NULL value of generic type.
null :: Value b a
null = NullVal

{-|
Value of generic type which can be used unquoted in a statement.
Note: such value is used "as is". For example, if you write:
> genVal True
It's going to be parsed as follow:
> True
The above is problematic with SqLite since it accepts only 1 or 0.
So, for this database you'ld need to write:
> genVal 1

To avoid this kind of issues it's better to use the specialised constructors.
In our present case:
> boolVal True

Then the value will be parsed appropriately depending on the database vendor.
-}
genVal :: (Raw c, Show c) => c -> Value b a
genVal = GenVal

-- | Value of generic type which needs to be quoted when used in a statement.
genQVal :: String -> Value b a
genQVal = GenQVal