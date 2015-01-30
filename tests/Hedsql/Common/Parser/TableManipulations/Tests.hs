module Hedsql.Common.Parser.TableManipulations.Tests where

import Hedsql.Common.Parser.TableManipulations.Create

import qualified Hedsql.SqLite     as S
import qualified Hedsql.PostgreSQL as P

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

-- | Gather all tests.
tests :: Test
tests = testGroup "Hedsql.Common.Parser.TableManipulations"
    [ testPrimaryKeyPostgreSQL
    , testCreateCheckSqLite
    , testCreateChecksSqLite
    , testCreateTableSqLite
    , testPrimaryKeySqLite
    , testPrimaryKeyTableSqLite 
    ]

testPrimaryKeyPostgreSQL :: Test
testPrimaryKeyPostgreSQL = testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for PostgreSQL"
            "CREATE TABLE \"People\" (\"id\" serial PRIMARY KEY)"
            (P.parse primaryKeyCol)

testCreateCheckSqLite :: Test
testCreateCheckSqLite = testCase "Create table with check" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Check constraint in table statement is incorrect"
            "CREATE TABLE \"People\" (\"age\" INTEGER CHECK (\"age\" > -1))"
            (S.parse createCheck)

testCreateChecksSqLite :: Test
testCreateChecksSqLite =
    testCase "Create table with many checks" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Check constraints in table statement are incorrect"
           ("CREATE TABLE \"People\" ("
         ++ "\"lastName\" VARCHAR(256), \"age\" INTEGER, "
         ++ "CONSTRAINT \"checks\" CHECK (\"age\" > -1 AND \"lastName\" <> '')"
         ++ ")")
           (S.parse createChecks)

testCreateTableSqLite :: Test
testCreateTableSqLite = testCase "Create table" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table statement is incorrect"
            "CREATE TABLE \"People\" (\"firstName\" VARCHAR(256))"
            (S.parse simpleTable)
           
testPrimaryKeySqLite :: Test
testPrimaryKeySqLite = testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for SqLite"
            "CREATE TABLE \"People\" (\"id\" INTEGER PRIMARY KEY AUTOINCREMENT)"
            (S.parse primaryKeyCol)
            
testPrimaryKeyTableSqLite :: Test
testPrimaryKeyTableSqLite = testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for SqLite"
           ("CREATE TABLE \"People\" ("
         ++ "\"firstName\" VARCHAR(256), "
         ++ "\"lastName\" VARCHAR(256), "
         ++ "CONSTRAINT \"pk\" PRIMARY KEY (\"firstName\", \"lastName\"))")
            (S.parse primaryKeyTable)