{-|
Module      : Database/Hedsql/Common/Constructor/Types.hs
Description : Constructor functions for SQL types.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for SQL types which can then be used in queries.
-}
module Database.Hedsql.Common.Constructor.Types
    (
      -- * Character types
      char
    , varchar
    
      -- * Numeric types
    , bigInt
    , integer
    , smallInt
    
      -- * Other types
    , boolean
    , date
    
      -- * Haskell data coercion types
      {-|
      Those types allow us to implement instances complying with the coverage
      condition when using instances using functional dependencies.

      More concretely we can define an instance such as:
      instance CoerceToType (SqlString a) (DataType a) where
      coerceToType = [...]
      -}
    , SqlBool
    , SqlInt
    , SqlString
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.DataStructure

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- TODO: check if they are used at all...
type SqlBool a = Bool
type SqlInt a = Int
type SqlString a = String

-- | Create a BOOLEAN.
boolean :: DataType Bool a
boolean = Bool

-- | Create a BIGINT.
bigInt :: DataType Numeric a
bigInt = BigInt

-- | Create a CHAR.
char :: Int -> DataType Text a
char = Char

-- | Create a DATE.
date :: DataType a Time
date = Date

-- | Create an INTEGER.
integer :: DataType Numeric a
integer = Integer

-- | Create a SMALLINT.
smallInt :: DataType Numeric a
smallInt = SmallInt

-- | Create a VARCHAR.
varchar :: Int -> DataType Text a
varchar = Varchar