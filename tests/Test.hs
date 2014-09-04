module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import qualified Hedsql.Helpers.MapTest

-- | Run the tests.
main = defaultMain tests

tests = [
    Hedsql.Helpers.MapTest.test
    ]