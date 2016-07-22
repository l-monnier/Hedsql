module Main where

import Test.Framework (defaultMain)

import Hedsql.Common.CodeGenerator.Quoter.Tests

-- | Run the tests.
main :: IO()
main = defaultMain
    [ Hedsql.Common.CodeGenerator.Quoter.Tests.tests
    ]
