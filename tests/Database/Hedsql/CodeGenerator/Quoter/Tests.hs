module Database.Hedsql.CodeGenerator.Quoter.Tests where

import Database.Hedsql.Common.CodeGenerator

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

-- | Create the default parser.
parser :: CodeGenerator a
parser = getCodeGenerator parser

-- | Gather all tests.
tests :: Test
tests = testGroup "Hedsql.Common.CodeGenerator.Quoter"
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
