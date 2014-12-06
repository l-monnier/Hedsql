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

Note that the functions and, max, min and sum exist also in the prelude.
It means that you need to use them with their qualified name or you need
to hide the functions of the prelude:
> import Prelude hiding (and, max, min, sum)
-}
module Hedsql.Common.Constructor.Functions
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
    , in_
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
import Hedsql.Helpers.Coerce

import Prelude hiding (and, max, min, sum)

-- Private functions.

-- Public functions.

-- | "+" operator.
(/+) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> Function c
(/+) left right = AddF $ Add (colRef left) (colRef right)

-- | "-" operator.
(/-) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> Function c
(/-) left right = SubstractF $ Substract (colRef left) (colRef right)

-- | "*" operator.
(/*) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> Function c
(/*) left right = MultiplyF $ Multiply (colRef left) (colRef right)

-- | Equality operator ("=" in SQL).
(/==) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
(/==) colRef1 colRef2 = EqualF $ Equal (colRef colRef1) (colRef colRef2)

-- | Greater than operator (">").
(/>) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
(/>) colRef1 colRef2 =
    GreaterThanF $ GreaterThan (colRef colRef1) (colRef colRef2)

-- | Greater than or equal to operator (">=").
(/>=) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
(/>=) colRef1 colRef2 =
    GreaterThanOrEqToF $ GreaterThanOrEqTo c1 c2
    where
        c1 = colRef colRef1
        c2 = colRef colRef2

-- | Smaller than operator ("<").
(/<) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
(/<) colRef1 colRef2 =
    SmallerThanF $ SmallerThan (colRef colRef1) (colRef colRef2)

-- | Smaller than or equal to operator ("<=").
(/<=) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
(/<=) colRef1 colRef2 =
    SmallerThanOrEqToF $ SmallerThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Unequality operator ("<>").
(/<>) :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
(/<>) colRef1 colRef2 = NotEqualF $ NotEqual (colRef colRef1) (colRef colRef2)

-- | Join two predicates with an AND.
and :: Coerce a (Condition b) => a -> a -> Condition b
and condition1 condition2 = And [condition condition1, condition condition2]

-- | Join a list of predicates with AND which will be enclosed in a parenthesis.
ands :: Coerce a (Condition b) => [a] -> Condition b
ands = And . map condition

-- | BETWEEN condition.
between ::
      (Coerce a [ColRef d], Coerce b [ColRef d], Coerce c [ColRef d])
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
count :: Coerce a [ColRef a] => a -> Function a
count = CountF . Count . expr

{- |
    Create a function which will return the current date.
    Its implementation shall vary depending on the vendor.
-}
currentDate :: Function a
currentDate = CurrentDateF CurrentDate

-- | Create an EXISTS function.
exists :: Coerce a [ColRef a] => a -> Condition a
exists = FuncCond . ExistsF . Exists . colRef

-- | Create an IN operator.
in_ :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
in_ colRef1 colRef2 = InF $ In (colRef colRef1) (colRef colRef2)

-- | Create a NOT IN operator.
notIn :: (Coerce a [ColRef c], Coerce b [ColRef c]) => a -> b -> FuncBool c
notIn colRef1 colRef2 = NotInF $ NotIn (colRef colRef1) (colRef colRef2)

-- | Create a IS FALSE function.
isFalse :: Coerce a [ColRef a] => a -> Condition a
isFalse = FuncCond . IsFalseF . IsFalse . colRef

-- | Create a IS NOT FALSE function.
isNotFalse :: Coerce a [ColRef a] => a -> Condition a
isNotFalse = FuncCond . IsNotFalseF . IsNotFalse . colRef

-- | Create a IS NOT NULL function.
isNotNull :: Coerce a [ColRef a] => a -> Condition a
isNotNull = FuncCond . IsNotNullF . IsNotNull . colRef

-- | Create a IS NOT TRUE function.
isNotTrue :: Coerce a [ColRef a] => a -> Condition a
isNotTrue = FuncCond . IsNotTrueF . IsNotTrue . colRef

-- | Create a IS NOT UNKNOWN function.
isNotUnknown :: Coerce a [ColRef a] => a -> Condition a
isNotUnknown = FuncCond . IsNotUnknownF . IsNotUnknown . colRef

-- | Create a IS NULL function.
isNull :: Coerce a [ColRef a] => a -> Condition a
isNull = FuncCond . IsNullF . IsNull . colRef

-- | Create a IS TRUE function.
isTrue :: Coerce a [ColRef a] => a -> Condition a
isTrue = FuncCond . IsTrueF . IsTrue . colRef

-- | Create a IS UNKNOWN function.
isUnknown :: Coerce a [ColRef a] => a -> Condition a
isUnknown = FuncCond . IsUnknownF . IsUnknown . colRef

-- | Create a MAX function.
max :: Coerce a [ColRef a] => a -> Function a
max = MaxF . Max . expr

-- | Create a MIN function.
min :: Coerce a [ColRef a] => a -> Function a
min = MinF . Min . expr

-- | NOT BETWEEN condition.
notBetween ::
      (Coerce a [ColRef d], Coerce b [ColRef d], Coerce c [ColRef d])
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
sum :: Coerce a [ColRef a] => a -> Function a
sum = SumF . Sum . expr