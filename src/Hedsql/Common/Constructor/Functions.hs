{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Hedsql/Common/Constructor/Functions.hs
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
module Hedsql.Common.Constructor.Functions
    ( (/+)
    , (/-)
    , (/*)
    , (/==)
    , (/>)
    , (/>=)
    , (/<)
    , (/<=)
    , (/<>)
    , and_
    , between
    , count
    , currentDate
    , exists
    , in_
    , isFalse
    , isNotFalse
    , isNotNull
    , isNotTrue
    , isNotUnknown
    , isNull
    , isTrue
    , isUnknown
    , max_
    , min_
    , notBetween
    , notIn
    , random
    , sum_
    ) where

import Hedsql.Common.Constructor.Columns
import Hedsql.Common.Constructor.Conditions
import Hedsql.Common.DataStructure

-- Private functions.

-- Public functions.

-- | "+" operator.
(/+) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> Function c
(/+) left right = AddF $ Add (colRef left) (colRef right)

-- | "-" operator.
(/-) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> Function c
(/-) left right = SubstractF $ Substract (colRef left) (colRef right)

-- | "*" operator.
(/*) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> Function c
(/*) left right = MultiplyF $ Multiply (colRef left) (colRef right)

-- | Equality operator ("=" in SQL).
(/==) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/==) colRef1 colRef2 = EqualF $ Equal (colRef colRef1) (colRef colRef2)

-- | Greater than operator (">").
(/>) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/>) colRef1 colRef2 =
    GreaterThanF $ GreaterThan (colRef colRef1) (colRef colRef2)

-- | Greater than or equal to operator (">=").
(/>=) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/>=) colRef1 colRef2 =
    GreaterThanOrEqToF $ GreaterThanOrEqTo c1 c2
    where
        c1 = colRef colRef1
        c2 = colRef colRef2

-- | Smaller than operator ("<").
(/<) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/<) colRef1 colRef2 =
    SmallerThanF $ SmallerThan (colRef colRef1) (colRef colRef2)

-- | Smaller than or equal to operator ("<=").
(/<=) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/<=) colRef1 colRef2 =
    SmallerThanOrEqToF $ SmallerThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Unequality operator ("<>").
(/<>) ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
(/<>) colRef1 colRef2 = NotEqualF $ NotEqual (colRef colRef1) (colRef colRef2)

-- | Join two predicates with an AND.
and_ :: CoerceToCondition a (Condition b) => a -> a -> Condition b
and_ condition1 condition2 = And [condition condition1, condition condition2]

-- | Join a list of predicates with AND which will be enclosed in a parenthesis.
ands :: CoerceToCondition a (Condition b) => [a] -> Condition b
ands = And . map condition

-- | BETWEEN condition.
between ::
    (
      CoerceToColRef a [ColRef d]
    , CoerceToColRef b [ColRef d]
    , CoerceToColRef c [ColRef d]
    )
    => a -- ^ Expression to evaluate.
    -> b -- ^ Lower bound condition.
    -> c -- ^ Higher bound condition.
    -> Condition d -- ^ Between condition.
between exp lower higher =
    FuncCond $ BetweenF $ Between exprRef lowerRef higherRef
    where
        exprRef = colRef exp
        lowerRef = colRef lower
        higherRef = colRef higher

-- | Create a COUNT function.
count :: CoerceToColRef a [ColRef b] => a -> Function b
count = CountF . Count . expr

{- |
    Create a function which will return the current date.
    Its implementation shall vary depending on the vendor.
-}
currentDate :: Function a
currentDate = CurrentDateF CurrentDate

-- | Create an EXISTS function.
exists :: CoerceToColRef a [ColRef b] => a -> Condition b
exists = FuncCond . ExistsF . Exists . colRef

-- | Create an IN operator.
in_ ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
in_ colRef1 colRef2 = InF $ In (colRef colRef1) (colRef colRef2)

-- | Create a NOT IN operator.
notIn ::
    (
      CoerceToColRef a [ColRef c]
    , CoerceToColRef b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
notIn colRef1 colRef2 = NotInF $ NotIn (colRef colRef1) (colRef colRef2)

-- | Create a IS FALSE function.
isFalse :: CoerceToColRef a [ColRef b] => a -> Condition b
isFalse = FuncCond . IsFalseF . IsFalse . colRef

-- | Create a IS NOT FALSE function.
isNotFalse :: CoerceToColRef a [ColRef b] => a -> Condition b
isNotFalse = FuncCond . IsNotFalseF . IsNotFalse . colRef

-- | Create a IS NOT NULL function.
isNotNull :: CoerceToColRef a [ColRef b] => a -> Condition b
isNotNull = FuncCond . IsNotNullF . IsNotNull . colRef

-- | Create a IS NOT TRUE function.
isNotTrue :: CoerceToColRef a [ColRef b] => a -> Condition b
isNotTrue = FuncCond . IsNotTrueF . IsNotTrue . colRef

-- | Create a IS NOT UNKNOWN function.
isNotUnknown :: CoerceToColRef a [ColRef b] => a -> Condition b
isNotUnknown = FuncCond . IsNotUnknownF . IsNotUnknown . colRef

-- | Create a IS NULL function.
isNull :: CoerceToColRef a [ColRef b] => a -> Condition b
isNull = FuncCond . IsNullF . IsNull . colRef

-- | Create a IS TRUE function.
isTrue :: CoerceToColRef a [ColRef b] => a -> Condition b
isTrue = FuncCond . IsTrueF . IsTrue . colRef

-- | Create a IS UNKNOWN function.
isUnknown :: CoerceToColRef a [ColRef b] => a -> Condition b
isUnknown = FuncCond . IsUnknownF . IsUnknown . colRef

-- | Create a MAX function.
max_ :: CoerceToColRef a [ColRef b] => a -> Function b
max_ = MaxF . Max . expr

-- | Create a MIN function.
min_ :: CoerceToColRef a [ColRef b] => a -> Function b
min_ = MinF . Min . expr

-- | NOT BETWEEN condition.
notBetween ::
    (
      CoerceToColRef a [ColRef d]
    , CoerceToColRef b [ColRef d]
    , CoerceToColRef c [ColRef d]
    )
    => a           -- ^ Expression to evaluate.
    -> b           -- ^ Lower bound condition.
    -> c           -- ^ Higher bound condition.
    -> Condition d -- ^ Not between condition.
notBetween expr lower higher =
    FuncCond $ NotBetweenF $ NotBetween exprRef lowerRef higherRef
    where
        exprRef = colRef expr
        lowerRef = colRef lower
        higherRef = colRef higher

-- | Create a random() function.
random :: Function a
random = RandomF Random
   
-- | Create a SUM function.
sum_ :: CoerceToColRef a [ColRef b] => a -> Function b
sum_ = SumF . Sum . expr