{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-} 

{-|
Module      : Database/Hedsql/Common/Constructor/Conditions.hs
Description : Constructor functions for conditions.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for conditions which can then be used in WHERE clauses
or for tables creation.
-}
module Database.Hedsql.Common.Constructor.Conditions
    ( ToConditions
    , condition
    , conditions
    , toCondition
    , toConditions
    ) where

import Database.Hedsql.Common.DataStructure

-- private functions.

-- | Coerce a given type to a list of conditions.
class ToConditions a b | a -> b where
    toConditions :: a -> b

instance ToConditions (Condition a) [Condition a] where
    toConditions c = [c]

instance ToConditions (FuncBool a) [Condition a] where
    toConditions c = [FuncCond c]

-- public functions.

-- | Create a condition.
condition :: ToConditions a [Condition b] => a -> Condition b
condition = toCondition

-- | Create a list of conditions from a list of element.
conditions :: ToConditions a [Condition b] => [a] -> [Condition b]
conditions = map toCondition

-- | Coerce a type to a condition.
toCondition :: ToConditions a [Condition b] => a -> Condition b
toCondition = head.toConditions