{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Database/Hedsql/Common/Constructor/Functions.hs
Description : Constructor functions for SQL functions.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for functions such as sum, count, etc. and
operators such as =, >, +, -, which can then be used in queries.

The operators have an additional slash before, so for example "/+" instead
of "+". It would have been possible to overide all the operators of the
prelude instead, but it would have been quite a clash!

Note that the functions and, max, min, in and sum exist also in the prelude.
Their name is therefore followed by an underscore.
-}
module Database.Hedsql.Common.Constructor.Functions
    (
      -- * Operators
      (/+)
    , (/-)
    , (/*)
    , (/==)
    , (/>)
    , (/>=)
    , (/<)
    , (/<=)
    , (/<>)
    
      -- * Comparison
    , between
    , exists
    , in_
    , like
    , notBetween
    , notIn
    
      -- * Logic
    , and_
    , ands
    -- TODO: or?
    
    -- * Conditions
    , isFalse
    , isNotFalse
    , isNotNull
    , isNotTrue
    , isNotUnknown
    , isNull
    , isTrue
    , isUnknown
    
    -- * Maths
    , count
    , max_
    , min_
    , random
    , sum_
    
    -- * Dates
    , currentDate
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------
    
import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.AST

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | "+" operator.
(/+) ::
    ( Num d
    , ToColRefs a [ColRef d c]
    , ToColRefs b [ColRef d c]
    )
    => a
    -> b
    -> Expression d c
(/+) left right = Add (colRef left) (colRef right)

-- | "-" operator.
(/-) ::
    ( Num d
    , ToColRefs a [ColRef d c]
    , ToColRefs b [ColRef d c]
    )
    => a
    -> b
    -> Expression d c
(/-) left right = Substract (colRef left) (colRef right)

-- | "*" operator.
(/*) ::
    ( Num d
    , ToColRefs a [ColRef d c]
    , ToColRefs b [ColRef d c]
    )
    => a
    -> b
    -> Expression d c
(/*) left right = Multiply (colRef left) (colRef right)

-- | Equality operator ("=" in SQL).
infix 7 /==
(/==) ::
    ( ToColRefs a [ColRef c d]
    , ToColRefs b [ColRef c d]
    )
    => a
    -> b
    -> Expression Bool d
(/==) colRef1 colRef2 = Equal (colRef colRef1) (colRef colRef2)

-- | Greater than operator (">").
infix 7 />
(/>) ::
    ( ToColRefs a [ColRef c d]
    , ToColRefs b [ColRef c d]
    )
    => a
    -> b
    -> Expression Bool d
(/>) colRef1 colRef2 = GreaterThan (colRef colRef1) (colRef colRef2)

-- | Greater than or equal to operator (">=").
infix 7 />=
(/>=) ::
    ( ToColRefs a [ColRef c d]
    , ToColRefs b [ColRef c d]
    )
    => a
    -> b
    -> Expression Bool d
(/>=) colRef1 colRef2 = GreaterThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Smaller than operator ("<").
infix 7 /<
(/<) ::
    ( ToColRefs a [ColRef c d]
    , ToColRefs b [ColRef c d]
    )
    => a
    -> b
    -> Expression Bool d
(/<) colRef1 colRef2 = SmallerThan (colRef colRef1) (colRef colRef2)

-- | Smaller than or equal to operator ("<=").
infix 7 /<=
(/<=) ::
    (
      SQLOrd c
    , ToColRefs a [ColRef c d]
    , ToColRefs b [ColRef c d]
    )
    => a
    -> b
    -> Expression Bool d
(/<=) colRef1 colRef2 = SmallerThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Unequality operator ("<>").
infix 7 /<>
(/<>) ::
    ( ToColRefs a [ColRef c d]
    , ToColRefs b [ColRef c d]
    )
    => a
    -> b
    -> Expression Bool d
(/<>) colRef1 colRef2 = NotEqual (colRef colRef1) (colRef colRef2)

-- | Join two predicates with an AND.
and_ :: Expression Bool b -> Expression Bool b -> Expression Bool b
and_ condition1 condition2 = And [condition1, condition2]

-- | Join a list of predicates with AND which will be enclosed in a parenthesis.
ands :: [Expression Bool b] -> Expression Bool b
ands = And 

-- | BETWEEN condition.
between ::
    (
      ToColRefs a [ColRef d e]
    , ToColRefs b [ColRef d e]
    , ToColRefs c [ColRef d e]
    )
    => a                 -- ^ Expression to evaluate.
    -> b                 -- ^ Lower bound condition.
    -> c                 -- ^ Higher bound condition.
    -> Expression Bool e -- ^ Between condition.
between ex lower higher = Between (colRef ex) (colRef lower) (colRef higher)

-- | Create a COUNT function.
count :: ToColRefs a [ColRef c b] => a -> Expression Int b 
count = Count . colRef

{- |
    Create a function which will return the current date.
    Its implementation shall vary depending on the vendor.
-}
currentDate :: Expression Time a
currentDate = CurrentDate

-- | Create an EXISTS function.
exists :: ToColRefs a [ColRef c b] => a -> Expression Bool b
exists = Exists . colRef

-- | Create an IN operator.
in_ ::
    (
      ToColRefs a [ColRef c   d]
    , ToColRefs b [ColRef [c] d]
    )
    => a
    -> b
    -> Expression Bool d
in_ colRef1 colRef2 = In (colRef colRef1) (colRef colRef2)

-- | Create a NOT IN operator.
notIn ::
    (
      ToColRefs a [ColRef c   d]
    , ToColRefs b [ColRef [c] d]
    )
    => a
    -> b
    -> Expression Bool d
notIn colRef1 colRef2 = NotIn (colRef colRef1) (colRef colRef2)

-- | Create a IS FALSE function.
isFalse :: ToColRefs a [ColRef Bool b] => a -> Expression Bool b
isFalse = IsFalse . colRef

-- | Create a IS NOT FALSE function.
isNotFalse :: ToColRefs a [ColRef Bool b] => a -> Expression Bool b
isNotFalse = IsNotFalse . colRef

-- | Create a IS NOT NULL function.
isNotNull :: ToColRefs a [ColRef c b] => a -> Expression Bool b
isNotNull = IsNotNull . colRef

-- | Create a IS NOT TRUE function.
isNotTrue :: ToColRefs a [ColRef Bool b] => a -> Expression Bool b
isNotTrue = IsNotTrue . colRef

-- | Create a IS NOT UNKNOWN function.
isNotUnknown :: ToColRefs a [ColRef c b] => a -> Expression Bool b
isNotUnknown = IsNotUnknown . colRef

-- | Create a IS NULL function.
isNull :: ToColRefs a [ColRef c b] => a -> Expression Bool b
isNull = IsNull . colRef

-- | Create a IS TRUE function.
isTrue :: ToColRefs a [ColRef Bool b] => a -> Expression Bool b
isTrue = IsTrue . colRef

-- | Create a IS UNKNOWN function.
isUnknown :: ToColRefs a [ColRef c b] => a -> Expression Bool b
isUnknown = IsUnknown . colRef

-- | Create a LIKE operator.
like ::
    (  ToColRefs a [ColRef String c]
    ,  ToColRefs b [ColRef String c]
    )
    => a
    -> b
    -> Expression Bool c
like colRef1 colRef2 = Like (colRef colRef1) (colRef colRef2)

-- | Create a MAX function.
max_ :: Num c => ToColRefs a [ColRef c b] => a -> Expression c b
max_ = Max . colRef

-- | Create a MIN function.
min_ :: Num c => ToColRefs a [ColRef c b] => a -> Expression c b
min_ = Min . colRef

-- | NOT BETWEEN condition.
notBetween ::
    (
      ToColRefs a [ColRef d e]
    , ToColRefs b [ColRef d e]
    , ToColRefs c [ColRef d e]
    )
    => a                 -- ^ Expression to evaluate.
    -> b                 -- ^ Lower bound condition.
    -> c                 -- ^ Higher bound condition.
    -> Expression Bool e -- ^ Not between condition.
notBetween ex lower higher =
    NotBetween (colRef ex) (colRef lower) (colRef higher)

-- | Create a random() function.
random :: Num b => Expression b a
random = Random
   
-- | Create a SUM function.
sum_ :: Num c => ToColRefs a [ColRef c b] => a -> Expression c b
sum_ = Sum . colRef