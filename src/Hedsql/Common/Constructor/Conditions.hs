{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Hedsql/Common/Constructor/Conditions.hs
Description : Constructor functions for conditions.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for conditions which can then be used in WHERE clauses
or for tables creation.
-}
module Hedsql.Common.Constructor.Conditions
    (
      ConditionConstruct
    , toCondition
    ) where

import Hedsql.Common.DataStructure.Base

import Unsafe.Coerce

-- private functions.

-- | Cast a FuncBool so it becomes generic again.
-- TODO: replace it with the safe coerce of coercible.  
castFuncBool :: FuncBool a -> FuncBool b
castFuncBool = unsafeCoerce

-- | Cast a Condition so it becomes generic again.
-- TODO: replace it with the safe coerce of coercible. 
castCondition :: Condition a -> Condition b
castCondition = unsafeCoerce

-- public functions.

-- | Convert a value to a condition.
class ConditionConstruct a where
    toCondition :: a -> Condition b

instance ConditionConstruct (Condition a) where
    toCondition a = castCondition a
    
instance ConditionConstruct (FuncBool a) where
    toCondition f = FuncCond $ castFuncBool f