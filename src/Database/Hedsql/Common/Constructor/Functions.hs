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
    
import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.Constructor.Conditions
import Database.Hedsql.Common.DataStructure

-- Private functions.

-- Public functions.

-- | "+" operator.
(/+) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> Function c
(/+) left right = Add (colRef left) (colRef right)

-- | "-" operator.
(/-) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> Function c
(/-) left right = Substract (colRef left) (colRef right)

-- | "*" operator.
(/*) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> Function c
(/*) left right = Multiply (colRef left) (colRef right)

-- | Equality operator ("=" in SQL). Infix 8.
infix 8 /==
(/==) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/==) colRef1 colRef2 = Equal (colRef colRef1) (colRef colRef2)

-- | Greater than operator (">").
(/>) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/>) colRef1 colRef2 = GreaterThan (colRef colRef1) (colRef colRef2)

-- | Greater than or equal to operator (">=").
(/>=) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/>=) colRef1 colRef2 = GreaterThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Smaller than operator ("<").
(/<) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/<) colRef1 colRef2 = SmallerThan (colRef colRef1) (colRef colRef2)

-- | Smaller than or equal to operator ("<=").
(/<=) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/<=) colRef1 colRef2 = SmallerThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Unequality operator ("<>").
(/<>) ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/<>) colRef1 colRef2 = NotEqual (colRef colRef1) (colRef colRef2)

-- | Join two predicates with an AND.
and_ :: ToConditions a [Condition b] => a -> a -> Condition b
and_ condition1 condition2 = And [condition condition1, condition condition2]

-- | Join a list of predicates with AND which will be enclosed in a parenthesis.
ands :: ToConditions a [Condition b] => [a] -> Condition b
ands = And . map condition

-- | BETWEEN condition.
between ::
    (
      ToColRefs a [ColRef d]
    , ToColRefs b [ColRef d]
    , ToColRefs c [ColRef d]
    )
    => a -- ^ Expression to evaluate.
    -> b -- ^ Lower bound condition.
    -> c -- ^ Higher bound condition.
    -> Condition d -- ^ Between condition.
between ex lower higher =
    FuncCond $ Between exprRef lowerRef higherRef
    where
        exprRef = colRef ex
        lowerRef = colRef lower
        higherRef = colRef higher

-- | Create a COUNT function.
count :: ToColRefs a [ColRef b] => a -> Function b
count = Count . expr

{- |
    Create a function which will return the current date.
    Its implementation shall vary depending on the vendor.
-}
currentDate :: Function a
currentDate = CurrentDate

-- | Create an EXISTS function.
exists :: ToColRefs a [ColRef b] => a -> Condition b
exists = FuncCond . Exists . colRef

-- | Create an IN operator.
in_ ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
in_ colRef1 colRef2 = In (colRef colRef1) (colRef colRef2)

-- | Create a NOT IN operator.
notIn ::
    (
      ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
notIn colRef1 colRef2 = NotIn (colRef colRef1) (colRef colRef2)

-- | Create a IS FALSE function.
isFalse :: ToColRefs a [ColRef b] => a -> Condition b
isFalse = FuncCond . IsFalse . colRef

-- | Create a IS NOT FALSE function.
isNotFalse :: ToColRefs a [ColRef b] => a -> Condition b
isNotFalse = FuncCond . IsNotFalse . colRef

-- | Create a IS NOT NULL function.
isNotNull :: ToColRefs a [ColRef b] => a -> Condition b
isNotNull = FuncCond . IsNotNull . colRef

-- | Create a IS NOT TRUE function.
isNotTrue :: ToColRefs a [ColRef b] => a -> Condition b
isNotTrue = FuncCond . IsNotTrue . colRef

-- | Create a IS NOT UNKNOWN function.
isNotUnknown :: ToColRefs a [ColRef b] => a -> Condition b
isNotUnknown = FuncCond . IsNotUnknown . colRef

-- | Create a IS NULL function.
isNull :: ToColRefs a [ColRef b] => a -> Condition b
isNull = FuncCond . IsNull . colRef

-- | Create a IS TRUE function.
isTrue :: ToColRefs a [ColRef b] => a -> Condition b
isTrue = FuncCond . IsTrue . colRef

-- | Create a IS UNKNOWN function.
isUnknown :: ToColRefs a [ColRef b] => a -> Condition b
isUnknown = FuncCond . IsUnknown . colRef

-- | Create a MAX function.
max_ :: ToColRefs a [ColRef b] => a -> Function b
max_ = Max . expr

-- | Create a MIN function.
min_ :: ToColRefs a [ColRef b] => a -> Function b
min_ = Min . expr

-- | NOT BETWEEN condition.
notBetween ::
    (
      ToColRefs a [ColRef d]
    , ToColRefs b [ColRef d]
    , ToColRefs c [ColRef d]
    )
    => a           -- ^ Expression to evaluate.
    -> b           -- ^ Lower bound condition.
    -> c           -- ^ Higher bound condition.
    -> Condition d -- ^ Not between condition.
notBetween ex lower higher =
    FuncCond $ NotBetween (colRef ex) (colRef lower) (colRef higher)

-- | Create a random() function.
random :: Function a
random = Random
   
-- | Create a SUM function.
sum_ :: ToColRefs a [ColRef b] => a -> Function b
sum_ = Sum . expr