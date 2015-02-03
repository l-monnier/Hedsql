module Hedsql.Common.Parser.TableManipulations.Tests where

import Hedsql.Common.Parser.TableManipulations.Create
import Hedsql.Common.Parser.TableManipulations.Drop

import qualified Database.Hedsql.SqLite     as S
import qualified Database.Hedsql.PostgreSQL as P

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

-- | Gather all tests.
tests :: Test
tests = testGroup "Hedsql.Common.Parser.TableManipulations"
    [ testPrimaryKeyAutoPostgreSQL
    , testPrimaryKeyPostgreSQL
    
    , testCreateCheckSqLite
    , testCreateChecksSqLite
    , testCreateFKSqLite
    , testCreateTableSqLite
    , testCreateUniqueSqLite
    , testCreateUniqueTSqLite
    , testDefaultValSqLite
    , testNoNullsSqLite
    , testPrimaryKeySqLite
    , testPrimaryKeyAutoSqLite
    , testPrimaryKeyTableSqLite
    
    , testDropTable
    , testDropTableIfExists
    ]

testPrimaryKeyPostgreSQL :: Test
testPrimaryKeyPostgreSQL = testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for PostgreSQL"
            "CREATE TABLE \"People\" (\"id\" integer PRIMARY KEY)"
            (P.parse primaryKeyCol)

testDefaultValSqLite :: Test
testDefaultValSqLite = testCase "Create table with a default value" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a default value"
            ("CREATE TABLE \"People\" (\"country\" VARCHAR(256) "
          ++ "DEFAULT('Switzerland'))")
            (S.parse defaultVal)
            
testNoNullsSqLite :: Test
testNoNullsSqLite =
    testCase "Create table with not null constraints" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with not null constraints"
            ("CREATE TABLE \"People\" ("
          ++ "\"firstName\" VARCHAR(256) CONSTRAINT \"no_null\" NOT NULL, "
          ++ "\"lastName\" VARCHAR(256) NOT NULL)")
            (S.parse noNulls)

testPrimaryKeyAutoPostgreSQL :: Test
testPrimaryKeyAutoPostgreSQL =
    testCase "Create table with primary key with auto increment" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            ("Create table with a primary key with auto increment"
          ++ "is incorrect for PostgreSQL")
            "CREATE TABLE \"People\" (\"id\" serial PRIMARY KEY)"
            (P.parse primaryKeyColAuto)

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

testCreateFKSqLite :: Test
testCreateFKSqLite =
    testCase "Create table with a foreign key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Foreign key in table statement is incorrect"
           ("CREATE TABLE \"People\" "
         ++ "(\"countryId\" INTEGER REFERENCES \"Countries\"(\"countryId\"))")
           (S.parse createFK)

testCreateTableSqLite :: Test
testCreateTableSqLite = testCase "Create table" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table statement is incorrect"
            "CREATE TABLE \"People\" (\"firstName\" VARCHAR(256))"
            (S.parse simpleTable)

testCreateUniqueSqLite :: Test
testCreateUniqueSqLite =
    testCase "Create table with unique constraint" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with unique constraint is incorrect"
            "CREATE TABLE \"People\" (\"passportNo\" VARCHAR(256) UNIQUE)"
            (S.parse createUnique)
            
testCreateUniqueTSqLite :: Test
testCreateUniqueTSqLite =
    testCase "Create table with unique constraint on two columns" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with unique constraint on two columns is incorrect"
           ("CREATE TABLE \"People\" (\"firstName\" VARCHAR(256), "
         ++ "\"lastName\" VARCHAR(256), UNIQUE (\"firstName\", \"lastName\"))")
            (S.parse createUniqueT)
  
testPrimaryKeySqLite :: Test
testPrimaryKeySqLite =
    testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for SqLite"
            "CREATE TABLE \"People\" (\"id\" INTEGER PRIMARY KEY)"
            (S.parse primaryKeyCol)
           
testPrimaryKeyAutoSqLite :: Test
testPrimaryKeyAutoSqLite =
    testCase "Create table with primary key and auto increment" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            ("Create table with a primary key with an auto increment"
          ++ "is incorrect for SqLite")
            "CREATE TABLE \"People\" (\"id\" INTEGER PRIMARY KEY AUTOINCREMENT)"
            (S.parse primaryKeyColAuto)
            
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
            
-- Drop statements.

testDropTable :: Test
testDropTable = testCase "Drop a table" assertDrop
    where
        assertDrop :: Assertion
        assertDrop = assertEqual
            "Drop table is incorrect for SqLite"
            "DROP TABLE \"People\""
            (S.parse dropTableStmt)
            
testDropTableIfExists :: Test
testDropTableIfExists = testCase "Drop a table if it exists" assertDrop
    where
        assertDrop :: Assertion
        assertDrop = assertEqual
            "Drop table if table exists is incorrect for SqLite"
            "DROP TABLE IF EXISTS \"People\""
            (S.parse dropTableIfExistsStmt)