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
    ( SqlInt
    , SqlString
    , bigInt
    , char
    , date
    , integer
    , smallInt
    , varchar
    ) where

import Database.Hedsql.Common.DataStructure

-- public functions.

{- |
Those types allow us to implement instances complying with the coverage
condition when using instances using functional dependencies.

More concretely we can define an instance such as:
> instance CoerceToType (SqlString a) (DataType a) where
>     coerceToType = [...]
-}
type SqlInt a = Int
type SqlString a = String

-- | Create a BIGINT
bigInt :: SqlDataType a
bigInt = BigInt

-- | Create a CHAR.
char :: Int -> SqlDataType a
char = Char

-- | Create a DATE.
date :: SqlDataType a
date = Date

-- | Create an INTEGER.
integer :: SqlDataType a
integer = Integer

-- | Create a SMALLINT.
smallInt :: SqlDataType a
smallInt = SmallInt

-- | Create a VARCHAR.
varchar :: Int -> SqlDataType a
varchar = Varchar