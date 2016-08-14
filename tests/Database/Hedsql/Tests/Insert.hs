{-# LANGUAGE OverloadedStrings #-}

module Database.Hedsql.Tests.Insert
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Data.Monoid
import Database.Hedsql.Statements.Insert

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

testWithCols :: Test
testWithCols = testCase "Insert with columns names" assertInsert
    where
        assertInsert :: Assertion
        assertInsert = assertEqual
            "Insert with columns names is incorrect"
            (  "INSERT INTO \"People\" "
            <> "(\"title\", \"firstName\", \"lastName\", \"age\", "
            <> "\"married\", \"passportNo\", \"countryId\") "
            <> "VALUES ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2)"
            )
            (S.codeGen withCols)

----------------------------------------
-- SqLite
----------------------------------------

testJulius :: Test
testJulius = testCase "Insert example Julius Ceasar" assertInsert
    where
        assertInsert :: Assertion
        assertInsert = assertEqual
            "Insert of Julius Ceasar is incorrect"
            (  "INSERT INTO \"People\" "
            <> "(\"id\", \"title\", \"firstName\", \"lastName\", \"age\", "
            <> "\"married\", \"passportNo\", \"father\", \"countryId\") "
            <> "VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, 1, NULL, 2, 2)"
            )
            (S.codeGen juliusCeasar)

----------------------------------------
-- PostgreSQL
----------------------------------------

testDefaultValPostgreSQL :: Test
testDefaultValPostgreSQL = testCase "Insert with a DEFAULT value" assertInsert
    where
        assertInsert :: Assertion
        assertInsert = assertEqual
            "Insert with a DEFAULT value is incorrect"
            (  "INSERT INTO \"People\" "
            <> "(\"id\", \"title\", \"firstName\", \"lastName\", \"age\", "
            <> "\"married\", \"passportNo\", \"father\", \"countryId\") "
            <> "VALUES (NULL, DEFAULT, 'Julius', 'Ceasar', 2000, "
            <> "TRUE, NULL, NULL, 2)"
            )
            (P.codeGen defaultValPostgreSQL)

testReturningPostgreSQL :: Test
testReturningPostgreSQL = testCase "Insert with RETURNING clause" assertInsert
    where
        assertInsert :: Assertion
        assertInsert = assertEqual
            "Insert with RETURNING clause is incorrect"
            (  "INSERT INTO \"People\" "
            <> "(\"title\", \"firstName\", \"lastName\", \"age\", "
            <> "\"married\", \"passportNo\", \"father\", \"countryId\") "
            <> "VALUES ('Mr', 'Julius', 'Ceasar', 2000, "
            <> "TRUE, NULL, 2, 2) "
            <> "RETURNING \"id\""
            )
            (P.codeGen returningPostgreSQL)

--testMultiValsPostgreSQL :: Test
--testMultiValsPostgreSQL = testCase "Multiple inserts" assertInsert
--    where
--        assertInsert :: Assertion
--        assertInsert = assertEqual
--            "Multiple inserts are incorrect"
--            (  "INSERT INTO \"People\" "
--            <> "(\"title\", \"firstName\", \"lastName\", \"age\", "
--            <> "\"passportNo\", \"countryId\") "
--            <> "VALUES ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2), "
--            <> "('Mr', 'Gnaeus', 'Pompeius', 2000, NULL, NULL, 2)"
--            )
--            (P.codeGen multiValsPostgreSQL)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Insert"
    [ testGroup "AllVendors"
        [ testWithCols
        ]
    , testGroup "SqLite"
        [ testJulius
        ]
    , testGroup "PostgreSQL"
        [ testDefaultValPostgreSQL
        , testReturningPostgreSQL
        --, testMultiValsPostgreSQL
        ]
    ]
