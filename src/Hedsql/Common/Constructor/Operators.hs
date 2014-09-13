{-|
Module      : Hedsql/Common/Constructor/Operators.hs
Description : Constructor functions for operators.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for operators such as =, >, +, -, which can then be used
in queries.
-}
module Hedsql.Common.Constructor.Operators
    (
      (/+)
    , (/-)
    , (/*)
    , (/==)
    , (/>)
    , (/>=)
    , (/<)
    , (/<=)
    , (/<>)
    , and
    , between
    , count
    , currentDate
    , exists
    , i
    , isFalse
    , isNotFalse
    , isNotNull
    , isNotTrue
    , isNotUnknown
    , isNull
    , isTrue
    , isUnknown
    , max
    , min
    , notBetween
    , notIn
    , random
    , sum
    ) where

import Hedsql.Common.Constructor.Columns
import Hedsql.Common.Constructor.Conditions
import Hedsql.Common.DataStructure.Base

import Prelude hiding (and, max, min, sum)

-- Private functions.

-- | Create a function condition using the provided function type.
isFunc :: ColRefConstruct a => (ColRef -> FunctionBoolean) -> a -> Condition
isFunc funcType colRef = FunctionCondition $ funcType $ toColRef colRef

-- Public functions.

-- | "+" operator.
(/+) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> Operator
(/+) left right = Add (toColRef left) (toColRef right)

-- | "-" operator.
(/-) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> Operator
(/-) left right = Substract (toColRef left) (toColRef right)

-- | "*" operator.
(/*) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> Operator
(/*) left right = Multiply (toColRef left) (toColRef right)

-- | Equality operator ("=" in SQL).
(/==) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
(/==) colRef1 colRef2 = Equal (toColRef colRef1) (toColRef colRef2)

-- | Greater than operator (">").
(/>) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
(/>) colRef1 colRef2 = GreaterThan (toColRef colRef1) (toColRef colRef2)

-- | Greater than or equal to operator (">=").
(/>=) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
(/>=) colRef1 colRef2 =
    GreaterThanOrEqualTo (toColRef colRef1) (toColRef colRef2)

-- | Smaller than operator ("<").
(/<) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
(/<) colRef1 colRef2 = SmallerThan (toColRef colRef1) (toColRef colRef2)

-- | Smaller than or equal to operator ("<=").
(/<=) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
(/<=) colRef1 colRef2 =
    SmallerThanOrEqualTo (toColRef colRef1) (toColRef colRef2)

-- | Unequality operator ("<>").
(/<>) :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
(/<>) colRef1 colRef2 = NotEqual (toColRef colRef1) (toColRef colRef2)

-- | Join two predicates with an AND.
and :: (ConditionConstruct a, ConditionConstruct b) => a -> b -> Condition
and condition1 condition2 = And [toCondition condition1, toCondition condition2]

-- | BETWEEN condition.
between ::
      (ColRefConstruct a, ColRefConstruct b, ColRefConstruct c)
    => a -- ^ Expression to evaluate.
    -> b -- ^ Lower bound condition.
    -> c -- ^ Higher bound condition.
    -> Condition -- ^ Between condition.
between expr lower higher =
    FunctionCondition $ Between exprRef lowerRef higherRef
    where
        exprRef = toColRef expr
        lowerRef = toColRef lower
        higherRef = toColRef higher

-- | Create a COUNT function.
count :: ColRefConstruct a => a -> Count
count expr = Count $ toExpr expr

{- |
    Create a function which will return the current date.
    Its implementation shall vary depending on the vendor.
-}
currentDate :: CurrentDate
currentDate = CurrentDate

-- | Create an EXISTS function.
exists :: ColRefConstruct a => a -> Condition
exists expr = FunctionCondition (Exists (toColRef expr))

-- | Create an IN operator.
i :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
i colRef1 colRef2 = In (toColRef colRef1) (toColRef colRef2)

-- | Create a NOT IN operator.
notIn :: (ColRefConstruct a, ColRefConstruct b) => a -> b -> FunctionBoolean
notIn colRef1 colRef2 = NotIn (toColRef colRef1) (toColRef colRef2)

-- | Create a IS FALSE function.
isFalse :: ColRefConstruct a => a -> Condition
isFalse colRef = FunctionCondition $ IsFalse $ toColRef colRef

-- | Create a IS NOT FALSE function.
isNotFalse :: ColRefConstruct a => a -> Condition
isNotFalse = isFunc IsNotFalse

-- | Create a IS NOT NULL function.
isNotNull :: ColRefConstruct a => a -> Condition
isNotNull = isFunc IsNotNull

-- | Create a IS NOT TRUE function.
isNotTrue :: ColRefConstruct a => a -> Condition
isNotTrue = isFunc IsNotTrue

-- | Create a IS NOT UNKNOWN function.
isNotUnknown :: ColRefConstruct a => a -> Condition
isNotUnknown = isFunc IsNotUnknown

-- | Create a IS NULL function.
isNull :: ColRefConstruct a => a -> Condition
isNull = isFunc IsNull

-- | Create a IS TRUE function.
isTrue :: ColRefConstruct a => a -> Condition
isTrue = isFunc IsTrue

-- | Create a IS UNKNOWN function.
isUnknown :: ColRefConstruct a => a -> Condition
isUnknown = isFunc IsUnknown

-- | Create a MAX function.
max :: ColRefConstruct a => a -> Max
max expr = Max $ toExpr expr

-- | Create a MIN function.
min :: ColRefConstruct a => a -> Min
min expr = Min $ toExpr expr

-- | NOT BETWEEN condition.
notBetween ::
      (ColRefConstruct a, ColRefConstruct b, ColRefConstruct c)
    => a -- ^ Expression to evaluate.
    -> b -- ^ Lower bound condition.
    -> c -- ^ Higher bound condition.
    -> Condition -- ^ Not between condition.
notBetween expr lower higher =
    FunctionCondition $ NotBetween exprRef lowerRef higherRef
    where
        exprRef = toColRef expr
        lowerRef = toColRef lower
        higherRef = toColRef higher

-- | Create a random() function.
random :: Random
random = Random
   
-- | Create a SUM function.
sum :: ColRefConstruct a => a -> Sum
sum expr = Sum $ toExpr expr