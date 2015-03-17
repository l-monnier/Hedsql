{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-} 
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Common/Constructor/DataManipulation.hs
Description : Wrapper class and instances.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Wrap a type "M a b" into a type "M a" using its wrapper.
For example:
> myWrap :: Column a b -> ColWrap a
> myWrap = wrap

This technic allows to build heteregeneous list of elements in a standardized
way with always the same function call.
-}
module Database.Hedsql.Common.Constructor.Wrapper
    ( wrap
    , wrapColRef
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.DataStructure

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

class Wrapper a d | a -> d where
    wrap :: a c b -> d b
    
instance Wrapper Column ColWrap where
    wrap = ColWrap

instance Wrapper ColRef ColRefWrap where
    wrap = ColRefWrap

instance Wrapper Select SelectWrap where
    wrap = SelectWrap
    
instance Wrapper Value ValueWrap where
    wrap = ValueWrap

-- | Create a column reference and wrap it, so it can be used in lists.
wrapColRef :: ToColRefs a [ColRef b c] => a -> ColRefWrap c    
wrapColRef = ColRefWrap . colRef