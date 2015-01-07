{-# LANGUAGE TemplateHaskell #-}

module Hedsql.Common.Parser.QuoterTest where
import Hedsql.Common.Parser.Quoter

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- | Gather all tests.
test =
    testGroup "Common.Parser.QuoterTest" [
          testProperty "Deletes" prop_deletes
        ]
        
-- | Test the idempotency property of the filterKeysWith function.
prop_genQuoterIde:: [Char] -> [String] -> Bool
prop_genQuoterIde quotes texts =
    -- TODO: find how to test it!
    genQuote quotes texts == genQuote quotes texts