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

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.DataStructure

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Coerce a given type to a list of boolean expressions.
class ToConditions a b | a -> b where
    toConditions :: a -> b

instance ToConditions (Expression Bool a) [Expression Bool a] where
    toConditions c = [c]

-- | Create a condition.
condition :: ToConditions a [Expression Bool b] => a -> Expression Bool b
condition = toCondition

-- | Create a list of conditions from a list of element.
conditions :: ToConditions a [Expression Bool b] => [a] -> [Expression Bool b]
conditions = map toCondition

-- | Coerce a type to a condition.
toCondition :: ToConditions a [Expression Bool b] => a -> Expression Bool b
toCondition = head.toConditions