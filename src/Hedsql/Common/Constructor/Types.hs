{-|
Module      : Hedsql/Common/Constructor/Types.hs
Description : Constructor functions for SQL types.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for SQL types which can then be used in queries.
-}
module Hedsql.Common.Constructor.Types
    (
      bigInt
    , char
    , date
    , integer
    , smallInt
    , varchar
    ) where

import Hedsql.Common.DataStructure.Base

-- private functions.

-- public functions.

-- | Create a BIGINT
bigInt :: SqlDataType
bigInt = BigInt

-- | Create a CHAR.
char :: Int -> SqlDataType
char = SqlChar

-- | Create a DATE.
date :: SqlDataType
date = Date

-- | Create an INTEGER.
integer :: SqlDataType
integer = Integer

-- | Create a SMALLINT.
smallInt :: SqlDataType
smallInt = SmallInt

-- | Create a VARCHAR.
varchar :: Int -> SqlDataType
varchar = Varchar