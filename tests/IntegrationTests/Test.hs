{-|
Module      : tests/IntegrationTests/Test.hs
Description : Integration testing.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Integration testing suite where queries created through Hedsql are directly
tested against a database.
-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
    
-- | Run the tests.
main :: IO()
main = defaultMain [testCase "Is Equal?" $ (@?=) "test" "test"]