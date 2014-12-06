{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

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
    condition
    ) where

import Hedsql.Common.DataStructure.Base
import Hedsql.Helpers.Coerce

import qualified Data.Coerce as C

-- private functions.

instance Coerce (Condition a) (Condition b) where
    coerce = C.coerce

instance Coerce (FuncBool a) (Condition b) where
    coerce func = FuncCond (C.coerce func)

-- public functions.

-- | Create a condition.
condition :: Coerce a (Condition b) => a -> Condition b
condition = coerce