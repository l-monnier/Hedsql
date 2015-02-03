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
    ( CoerceToCondition
    , condition
    ) where

import Database.Hedsql.Common.DataStructure

-- private functions.

-- | Coerce a given type to a Condition.
class CoerceToCondition a b | a -> b where
    coerceToCondition :: a -> b

instance CoerceToCondition (Condition a) (Condition a) where
    coerceToCondition = id

instance CoerceToCondition (FuncBool a) (Condition a) where
    coerceToCondition = FuncCond

-- public functions.

-- | Create a condition.
condition :: CoerceToCondition a (Condition b) => a -> Condition b
condition = coerceToCondition