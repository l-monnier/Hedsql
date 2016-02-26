module Hedsql.Common.Parser.Quoter.Tests where

import Database.Hedsql.Common.Parser

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

-- | Create the default parser.
parser :: Parser a
parser = getParser parser

-- | Gather all tests.
tests :: Test
tests = testGroup "Hedsql.Common.Parser.Quoter"
    [ testBasicQuoteElem
    , testBasicQuoteVal
    , testQuotedQuoteElem
    , testQuotedQuoteVal
    ]

testBasicQuoteElem :: Test
testBasicQuoteElem = testCase "Quote basic element" assertGoodQuote
    where
        assertGoodQuote :: Assertion
        assertGoodQuote = assertEqual
            "Basic element not correctly quoted"
            "\"test\""
            (show $ _quoteElem parser "test")

testBasicQuoteVal :: Test
testBasicQuoteVal = testCase "Quote basic value" assertGoodQuote
    where
        assertGoodQuote :: Assertion
        assertGoodQuote = assertEqual
            "Basic value not correctly quoted"
            "'test'"
            (show $ _quoteVal parser "test")

testQuotedQuoteElem :: Test
testQuotedQuoteElem = testCase "Quote a quoted element" assertGoodQuote
    where
        assertGoodQuote :: Assertion
        assertGoodQuote = assertEqual
            "Quoted element not correctly quoted"
            "\"\"\"test\"\"\""
            (show $ _quoteElem parser "\"test\"")

testQuotedQuoteVal :: Test
testQuotedQuoteVal = testCase "Quote quoted value" assertGoodQuote
    where
        assertGoodQuote :: Assertion
        assertGoodQuote = assertEqual
            "Quoted value not correctly quoted"
            "'''test'''"
            (show $ _quoteVal parser "'test'")
