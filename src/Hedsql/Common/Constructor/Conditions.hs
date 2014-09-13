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

-- private functions.

-- public functions.

-- | Convert a value to a condition.
class ConditionConstruct a where
    toCondition :: a -> Condition

instance ConditionConstruct Condition where
    toCondition a = a
    
instance ConditionConstruct FunctionBoolean where
    toCondition = FunctionCondition