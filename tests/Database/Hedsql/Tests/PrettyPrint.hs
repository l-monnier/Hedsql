{-# LANGUAGE OverloadedStrings #-}

module Database.Hedsql.Tests.PrettyPrint
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.Statements.Create
import           Database.Hedsql.Statements.Delete
import           Database.Hedsql.Statements.Insert as Insert
import           Database.Hedsql.Statements.Select
import           Database.Hedsql.Statements.Update

import           Test.Framework                    (Test, testGroup)
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        hiding (Test)

import qualified Database.Hedsql.SqLite            as S

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- SELECT

testSelectStruct :: Test
testSelectStruct = testCase "Basic SELECT structure" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT structure is incorrect."
            "SELECT *\n\
            \FROM \"People\"\n\
            \WHERE \"age\" > 18\n\
            \GROUP BY \"lastName\"\n\
            \HAVING SUM(\"age\") > 100\n\
            \ORDER BY \"id\"\n\
            \LIMIT 30 OFFSET 2"
            (S.codeGenP selectFull)

testSelectOneCol :: Test
testSelectOneCol = testCase "SELECT with one column" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT with one column is incorrect."
            "SELECT *\n\
            \FROM \"People\""
            (S.codeGenP selectAll)

testSelectTwoCols :: Test
testSelectTwoCols = testCase "SELECT with two columns" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT with two columns is incorrect."
            "SELECT\n\
            \  \"firstName\",\n\
            \  \"lastName\"\n\
            \FROM \"People\""
            (S.codeGenP selectTwoCols)

testSelectCombinedOne :: Test
testSelectCombinedOne =
    testCase "Combined SELECT statement with one combination" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Combined SELECT statement with one combination is incorrect."
            "SELECT *\n\
            \FROM \"People\"\n\
            \WHERE \"personId\" = 1\n\
            \UNION\n\
            \SELECT *\n\
            \FROM \"People\"\n\
            \WHERE \"personId\" = 2"
            (S.codeGenP unionQuery)

testJoin :: Test
testJoin =
    testCase "SELECT with a join clause" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT with a join clause is incorrect."
            "SELECT *\n\
            \FROM \"People\"\n\
            \INNER JOIN \"Countries\"\n\
            \ON \"People\".\"countryId\" = \"Countries\".\"countryId\""
            (S.codeGenP fromInnerJoinOn)

testNestedJoins :: Test
testNestedJoins =
    testCase "SELECT with a nested join clause" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT with a nested join clause is incorrect."
            "SELECT *\n\
            \FROM \"People\"\n\
            \INNER JOIN \"Countries\"\n\
            \ON \"People\".\"countryId\" = \"Countries\".\"countryId\"\n\
            \INNER JOIN \"Addresses\"\n\
            \ON \"People\".\"personId\" = \"Addresses\".\"personId\""
            (S.codeGenP nestedJoins)

testSubQueryFrom :: Test
testSubQueryFrom =
    testCase "FROM with a sub-query." assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "FROM with a sub-query is incorrect."
            "SELECT *\n\
            \FROM (SELECT *\n\
            \      FROM \"People\") AS \"P\""
            (S.codeGenP selectSubQuery)

testSubQueryWhere :: Test
testSubQueryWhere =
    testCase "WHERE with a sub-query." assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "WHERE with a sub-query is incorrect."
            "SELECT *\n\
            \FROM \"People\"\n\
            \WHERE \"countryId\" IN (SELECT \"countryId\"\n\
            \                      FROM \"Countries\"\n\
            \                      WHERE \"inhabitants\" >= \"size\" * 100)"
            (S.codeGenP whereInSelect)

testWhereAnd :: Test
testWhereAnd =
    testCase "WHERE with an AND clause." assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "WHERE with an AND clause is incorrect."
            "SELECT *\n\
            \FROM\n\
            \  \"People\",\n\
            \  \"Countries\"\n\
            \WHERE\n\
            \  \"People\".\"countryId\" = \"Countries\".\"countryId\"\n\
            \  AND \"People\".\"age\" > 18"
            (S.codeGenP whereAnd)

testWhereAnds :: Test
testWhereAnds =
    testCase "WHERE with two AND clauses." assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "WHERE with two AND clauses is incorrect."
            "SELECT *\n\
            \FROM\n\
            \  \"People\",\n\
            \  \"Countries\"\n\
            \WHERE\n\
            \  \"People\".\"countryId\" = \"Countries\".\"countryId\"\n\
            \  AND \"People\".\"age\" > 18\n\
            \  AND \"People\".\"age\" < 70"
            (S.codeGenP whereAnds)

testGroupByOne :: Test
testGroupByOne =
    testCase "GROUP BY with one column" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "GROUP BY with one column is incorrect."
            "SELECT \"age\"\n\
            \FROM \"People\"\n\
            \GROUP BY \"age\""
            (S.codeGenP selectGroupBy)

testGroupByTwo :: Test
testGroupByTwo =
    testCase "GROUP BY with two columns" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "GROUP BY with two columns is incorrect."
            "SELECT\n\
            \  \"firstName\",\n\
            \  \"age\"\n\
            \FROM \"People\"\n\
            \GROUP BY\n\
            \  \"firstName\",\n\
            \  \"age\""
            (S.codeGenP groupByTwo)

testHavingOne :: Test
testHavingOne =
    testCase "Having with one condition" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Having with one condition is incorrect."
            "SELECT\n\
            \  \"lastName\",\n\
            \  SUM(\"age\")\n\
            \FROM \"People\"\n\
            \GROUP BY \"lastName\"\n\
            \HAVING SUM(\"age\") > 18"
            (S.codeGenP groupBySumHaving)

testHavingTwo :: Test
testHavingTwo =
    testCase "Having with two conditions" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Having with two conditions is incorrect."
            "SELECT \"firstName\"\n\
            \FROM \"People\"\n\
            \GROUP BY \"firstName\"\n\
            \HAVING\n\
            \  SUM(\"age\") > 18\n\
            \  OR SUM(\"size\") < 1800"
            (S.codeGenP groupBySumHavingTwo)

testOrderByOne :: Test
testOrderByOne =
    testCase "ORDER BY with one column" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "ORDER BY with one column is incorrect."
            "SELECT \"firstName\"\n\
            \FROM \"People\"\n\
            \ORDER BY \"firstName\""
            (S.codeGenP orderByQuery)

testOrderByTwo :: Test
testOrderByTwo =
    testCase "ORDER BY with two columns" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "ORDER BY with two columns is incorrect."
            "SELECT\n\
            \  \"firstName\",\n\
            \  \"lastName\"\n\
            \FROM \"People\"\n\
            \ORDER BY\n\
            \  \"firstName\" ASC,\n\
            \  \"lastName\" DESC"
            (S.codeGenP orderByAscDesc)

testLimit :: Test
testLimit =
    testCase "LIMIT clause." assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "LIMIT clause is incorrect."
            "SELECT *\n\
            \FROM \"People\"\n\
            \ORDER BY \"firstName\"\n\
            \LIMIT 2"
            (S.codeGenP orderByLimit)

testLimitOffset :: Test
testLimitOffset =
    testCase "LIMIT and OFFSET clauses." assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "LIMIT and OFFSET clauses are incorrect."
            "SELECT *\n\
            \FROM \"People\"\n\
            \ORDER BY \"firstName\"\n\
            \LIMIT 5 OFFSET 2"
            (S.codeGenP orderByLimitOffset)

-- CREATE

testCreate :: Test
testCreate =
    testCase "CREATE statement." assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "CREATE statement incorrect."
            "CREATE TABLE \"Countries\" (\n\
            \  \"countryId\"   INTEGER PRIMARY KEY AUTOINCREMENT,\n\
            \  \"name\"        VARCHAR(256) NOT NULL, UNIQUE,\n\
            \  \"size\"        INTEGER,\n\
            \  \"inhabitants\" INTEGER\n\
            \)"
            (S.codeGenP countries)

-- UPDATE

testUpdate :: Test
testUpdate =
    testCase "UPDATE statement." assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "UPDATE statement incorrect."
            "UPDATE \"People\"\n\
            \SET \"age\" = 2050\n\
            \WHERE \"lastName\" = 'Ceasar'"
            (S.codeGenP equalTo)

-- DELETE

testDelete :: Test
testDelete =
    testCase "DELETE statement." assertDelete
    where
        assertDelete :: Assertion
        assertDelete = assertEqual
            "DELETE statement incorrect."
            "DELETE FROM \"People\"\n\
            \WHERE \"age\" <> 20"
            (S.codeGenP deleteNotEqualTo)

-- INSERT

testInsert :: Test
testInsert =
    testCase "INSERT statement." assertInsert
    where
        assertInsert :: Assertion
        assertInsert = assertEqual
            "Insert statement incorrect."
            "INSERT INTO \"People\" (\n\
            \  \"firstName\",\n\
            \  \"lastName\",\n\
            \  \"age\",\n\
            \  \"passportNo\",\n\
            \  \"countryId\")\n\
            \VALUES (\n\
            \  'Julius',\n\
            \  'Ceasar',\n\
            \  2000,\n\
            \  NULL,\n\
            \  2)"
            (S.codeGenP Insert.defaultVal)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Pretty Print"
    [ testSelectStruct
    , testSelectOneCol
    , testSelectTwoCols
    , testSelectCombinedOne
    , testJoin
    , testNestedJoins
    , testSubQueryFrom
    , testSubQueryWhere
    , testWhereAnd
    , testWhereAnds
    , testGroupByOne
    , testGroupByTwo
    , testHavingOne
    , testHavingTwo
    , testOrderByOne
    , testOrderByTwo
    , testLimit
    , testLimitOffset
    , testCreate
    , testUpdate
    , testDelete
    , testInsert
    ]
