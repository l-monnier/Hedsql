{-# LANGUAGE OverloadedStrings #-}

module Database.Hedsql.Tests.Update
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Data.Monoid
import Database.Hedsql.Statements.Update

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

import qualified Database.Hedsql.PostgreSQL as P
import qualified Database.Hedsql.SqLite     as S

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

----------------------------------------
-- All vendors
----------------------------------------

testEqualTo :: Test
testEqualTo = testCase "Update with equal-to" assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "Update with equal-to is incorrect"
            "UPDATE \"People\" SET \"age\" = 2050 WHERE \"lastName\" = 'Ceasar'"
            (S.codeGen equalTo)

testUpdateSelect :: Test
testUpdateSelect = testCase "Update with a SELECT" assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "Update with a SELECT is incorrect"
            (  "UPDATE \"People\" SET \"age\" = \"age\" + 1 "
            <> "WHERE \"countryId\" IN "
            <> "(SELECT \"countryId\" FROM \"Countries\" "
            <> "WHERE \"name\" = 'Italy')"
            )
            (S.codeGen updateSelect)

----------------------------------------
-- PostgreSQL
----------------------------------------

testDefaultVal :: Test
testDefaultVal = testCase "Update with defaultValue" assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "Update with a default value is incorrect"
            "UPDATE \"People\" SET \"title\" = DEFAULT WHERE \"personId\" = 1"
            (P.codeGen defaultVal)

testReturning :: Test
testReturning = testCase "Update with RETURNING clause" assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "Update with a RETURNING clause is incorrect"
            (   "UPDATE \"People\" SET \"age\" = 2050 "
            <>  "WHERE \"personId\" = 1 "
            <>  "RETURNING \"personId\""
            )
            (P.codeGen updateReturningClause)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Update"
    [ testGroup "All vendors"
        [ testEqualTo
        , testUpdateSelect
        ]
    , testGroup "PostgreSQL"
        [ testDefaultVal
        , testReturning
        ]
    ]
