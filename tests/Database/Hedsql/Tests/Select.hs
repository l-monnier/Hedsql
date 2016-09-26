{-# LANGUAGE OverloadedStrings #-}

module Database.Hedsql.Tests.Select
    ( tests
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Monoid
import Database.Hedsql.Statements.Select

import Data.Text.Lazy                 ()
import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

import qualified Database.Hedsql.PostgreSQL as P
import qualified Database.Hedsql.MariaDB    as M
import qualified Database.Hedsql.SqLite     as S

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

----------------------------------------
-- SELECT
----------------------------------------

testSelectAllSqLite :: Test
testSelectAllSqLite = testCase "Select all" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select all query is incorrect"
            "SELECT * FROM \"People\""
            (S.codeGen selectAll)

testSelectDistinctSqLite :: Test
testSelectDistinctSqLite = testCase "Select distinct" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select distinct query is incorrect"
            "SELECT DISTINCT \"firstName\" FROM \"People\""
            (S.codeGen distinctSelect)

testSelectTuple :: Test
testSelectTuple = testCase "Select 2-tuple" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select 2-tuple query is incorrect"
            "SELECT \"firstName\", \"age\" FROM \"People\""
            (S.codeGen selectTuple)

testSelect3Tuple :: Test
testSelect3Tuple = testCase "Select 3-tuple" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select 3-tuple query is incorrect"
            "SELECT \"firstName\", \"lastName\", \"age\" FROM \"People\""
            (S.codeGen select3Tuple)

testSelect4Tuple :: Test
testSelect4Tuple = testCase "Select 4-tuple" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select 4-tuple query is incorrect"
            (   "SELECT \"firstName\", \"lastName\", \"age\", \"title\" "
             <> "FROM \"People\""
            )
            (S.codeGen select4Tuple)

----------------------------------------
-- Functions
----------------------------------------

testAdditionSqLite :: Test
testAdditionSqLite = testCase "Addition" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Addition in query is incorrect"
            "SELECT \"age\" + 1 FROM \"People\""
            (S.codeGen addition)

testMultiplicationSqLite :: Test
testMultiplicationSqLite = testCase "Multiplication" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Multiplication in query is incorrect"
            "SELECT 3 * 4"
            (S.codeGen multiplication)

testCurrentDateSqLite :: Test
testCurrentDateSqLite = testCase "Current date" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Current date function in query is incorrect"
            "SELECT Date('now')"
            (S.codeGen selectCurrentDate)

testRandomSqLite :: Test
testRandomSqLite = testCase "Random" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Random function in query is incorrect"
            "SELECT random()"
            (S.codeGen selectRandom)

testTrim :: Test
testTrim = testCase "Trim" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "TRIM function in query is incorrect"
            "SELECT TRIM(\"name\")"
            (S.codeGen selectTrim)


testLastInsertIdPostgreSQL :: Test
testLastInsertIdPostgreSQL =
    testCase "Last Insert ID for PostgresQL" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Last Insert ID for PostgreSQL is incorrect"
            "SELECT lastval()"
            (P.codeGen selectLastInsertId)

testLastInsertIdMariaDB :: Test
testLastInsertIdMariaDB = testCase "Last Insert ID for MariaDB" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Last Insert ID for MariaDB is incorrect"
            "SELECT LAST_INSERT_ID()"
            (M.codeGen selectLastInsertId)

testLastInsertIdSqLite :: Test
testLastInsertIdSqLite = testCase "Last Insert ID for SqLite" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Last Insert ID for SqLite is incorrect"
            "SELECT last_insert_rowid()"
            (S.codeGen selectLastInsertId)

----------------------------------------
-- FROM
----------------------------------------

testCrossJoinSqLite :: Test
testCrossJoinSqLite = testCase "Cross join" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Cross join is incorrect"
            "SELECT * FROM \"People\" CROSS JOIN \"Countries\""
            (S.codeGen fromCrossJoin)

testInnerJoinOnSqLite :: Test
testInnerJoinOnSqLite = testCase "Inner join SqLite" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "SqLite inner join is incorrect"
           ("SELECT * "
         <> "FROM \"People\" "
         <> "INNER JOIN \"Countries\" "
         <> "ON \"People\".\"countryId\" = \"Countries\".\"countryId\"")
            (S.codeGen fromInnerJoinOn)

testInnerJoinUsingSqLite :: Test
testInnerJoinUsingSqLite = testCase "Inner join USING SqLite" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "SqLite inner join using is incorrect"
           ("SELECT * "
         <> "FROM \"People\" INNER JOIN \"Countries\" USING (\"countryId\")")
            (S.codeGen fromInnerJoinUsing)

testNaturalInnerJoin :: Test
testNaturalInnerJoin = testCase "Natural inner join" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Natural inner join is incorrect"
            "SELECT * FROM \"People\" NATURAL INNER JOIN \"Countries\""
            (S.codeGen fromNaturalInnerJoin)

testLeftJoinOn :: Test
testLeftJoinOn = testCase "Left join on" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Left join on is incorrect"
            (  "SELECT * FROM \"People\" LEFT JOIN \"Countries\" "
            <> "ON \"People\".\"countryId\" = \"Countries\".\"countryId\""
            )
            (S.codeGen fromLeftJoinOn)

testLeftJoinUsing :: Test
testLeftJoinUsing = testCase "Left join using" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Left join using is incorrect"
            (  "SELECT * FROM \"People\" LEFT JOIN \"Countries\" "
            <> "USING (\"countryId\")"
            )
            (S.codeGen fromLeftJoinUsing)

testRightJoinOn :: Test
testRightJoinOn = testCase "Right join on" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Right join on is incorrect"
            (  "SELECT * FROM \"People\" RIGHT JOIN \"Countries\" "
            <> "ON \"People\".\"countryId\" = \"Countries\".\"countryId\""
            )
            (S.codeGen fromRightJoinOn)

testFullJoinOn :: Test
testFullJoinOn = testCase "Full join on" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Full join on is incorrect"
            (  "SELECT * FROM \"People\" FULL JOIN \"Countries\" "
            <> "ON \"People\".\"countryId\" = \"Countries\".\"countryId\""
            )
            (S.codeGen fromFullJoinOn)

testLeftJoinOnAnd :: Test
testLeftJoinOnAnd = testCase "Left join on and" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Left join on and is incorrect"
            (  "SELECT * FROM \"People\" LEFT JOIN \"Countries\" "
            <> "ON (\"People\".\"countryId\" = \"Countries\".\"countryId\" "
            <> "AND \"Countries\".\"name\" = 'Italy')"
            )
            (S.codeGen fromLeftJoinOnAnd)

testSelfJoin :: Test
testSelfJoin = testCase "Self join" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Self join is incorrect"
            (  "SELECT * FROM \"People\" AS \"Father\" "
            <> "INNER JOIN \"People\" AS \"Child\" "
            <> "ON \"Father\".\"personId\" = \"Child\".\"father\""
            )
            (S.codeGen selfJoin)

testCrossJoinAlias :: Test
testCrossJoinAlias = testCase "Cross join with aliases" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Cross join with aliases is incorrect"
            (  "SELECT * FROM \"People\" AS \"P\" "
            <> "CROSS JOIN \"Countries\" AS \"C\""
            )
            (S.codeGen crossJoinAlias)

testCrossRefAlias :: Test
testCrossRefAlias = testCase "Cross join alias reference" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Cross join alias reference is incorrect"
            (  "SELECT * FROM (\"People\" AS \"P\" "
            <> "CROSS JOIN \"Countries\") AS \"PC\""
            )
            (S.codeGen crossRefAlias)

testSubQuery :: Test
testSubQuery = testCase "Sub-query in FROM clause" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Sub-query in FROM clause is incorrect"
            "SELECT * FROM (SELECT * FROM \"People\") AS \"P\""
            (S.codeGen selectSubQuery)

testNestedJoins :: Test
testNestedJoins = testCase "Multiple joins in FROM clause" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Multiple joins in FROM clause is incorrect"
           ("SELECT * "
         <> "FROM \"People\" "
         <> "INNER JOIN \"Countries\" "
         <> "ON \"People\".\"countryId\" = \"Countries\".\"countryId\" "
         <> "INNER JOIN \"Addresses\" "
         <> "ON \"People\".\"personId\" = \"Addresses\".\"personId\"")
            (S.codeGen nestedJoins)

----------------------------------------
-- WHERE
----------------------------------------

testWhereAlias :: Test
testWhereAlias = testCase "WHERE clause with aliases" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with aliases is incorrect"
            "SELECT * FROM \"People\" AS \"P\" WHERE \"P\".\"age\" > 5"
            (S.codeGen whereAlias)

testWhereAnd :: Test
testWhereAnd = testCase "WHERE clause with AND" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with AND is incorrect"
            (  "SELECT * FROM \"People\", \"Countries\" "
            <> "WHERE \"People\".\"countryId\" = \"Countries\".\"countryId\" "
            <> "AND \"People\".\"age\" > 18"
            )
            (S.codeGen whereAnd)

testWhereInValues :: Test
testWhereInValues = testCase "WHERE clause with IN values" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with IN values is incorrect"
            (  "SELECT * FROM \"Countries\" "
            <> "WHERE \"name\" IN ('Italy', 'Switzerland')"
            )
            (S.codeGen whereInValues)

testWhereInSelect :: Test
testWhereInSelect = testCase "WHERE clause with IN sub-query" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with IN sub-query is incorrect"
            (  "SELECT * FROM \"People\" "
            <> "WHERE \"countryId\" IN (SELECT \"countryId\" "
            <> "FROM \"Countries\" WHERE \"inhabitants\" >= \"size\" * 100)"
            )
            (S.codeGen whereInSelect)

testWhereBetween :: Test
testWhereBetween = testCase "WHERE clause with BETWEEN clause" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with BETWEEN clause is incorrect"
            (  "SELECT * FROM \"Countries\" "
            <> "WHERE \"inhabitants\" BETWEEN 10000 AND 1000000"
            )
            (S.codeGen whereBetween)

testWhereExists :: Test
testWhereExists = testCase "WHERE clause with EXISTS sub-query" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with EXISTS sub-query is incorrect"
            (  "SELECT * FROM \"People\" "
            <> "WHERE EXISTS (SELECT * "
            <> "FROM \"Countries\" "
            <> "WHERE \"People\".\"countryId\" = \"Countries\".\"countryId\")"
            )
            (S.codeGen whereExists)

----------------------------------------
-- ORDER BY
----------------------------------------

testOrderBy :: Test
testOrderBy = testCase "ORDER BY clause" assertOrderBy
    where
        assertOrderBy :: Assertion
        assertOrderBy = assertEqual
            "ORDER BY clause is incorrect"
            (  "SELECT \"firstName\" FROM \"People\" "
            <> "ORDER BY \"firstName\""
            )
            (S.codeGen orderByQuery)

testOrderByAlias :: Test
testOrderByAlias = testCase "ORDER BY alias clause" assertOrderBy
    where
        assertOrderBy :: Assertion
        assertOrderBy = assertEqual
            "ORDER BY alias clause is incorrect"
            (  "SELECT \"size\" + \"inhabitants\" AS \"sum\", \"name\" "
            <> "FROM \"Countries\" ORDER BY \"sum\""
            )
            (S.codeGen orderBySum)

testOrderByAscDesc :: Test
testOrderByAscDesc = testCase "ORDER BY clause with ASC and DESC" assertOrderBy
    where
        assertOrderBy :: Assertion
        assertOrderBy = assertEqual
            "ORDER BY clause with ASC and DESC is incorrect"
            (  "SELECT \"firstName\", \"lastName\" FROM \"People\" "
            <> "ORDER BY \"firstName\" ASC, \"lastName\" DESC"
            )
            (S.codeGen orderByAscDesc)

testOrderByNull :: Test
testOrderByNull =
    testCase "ORDER BY clause with NULLS first and last" assertOrderBy
    where
        assertOrderBy :: Assertion
        assertOrderBy = assertEqual
            "ORDER BY clause with NullS first and last is incorrect"
            (  "SELECT \"age\", \"passeportNumber\" FROM \"People\" "
            <> "ORDER BY \"age\" NULLS FIRST, \"passeportNumber\" NULLS LAST"
            )
            (S.codeGen orderByNull)

testOrderByLimit :: Test
testOrderByLimit = testCase "ORDER BY with LIMIT clause" assertOrderBy
    where
        assertOrderBy :: Assertion
        assertOrderBy = assertEqual
            "ORDER BY with LIMIT clause is incorrect"
            (  "SELECT * FROM \"People\" "
            <> "ORDER BY \"firstName\" LIMIT 2"
            )
            (S.codeGen orderByLimit)

----------------------------------------
-- GROUP BY
----------------------------------------

testGroupBy :: Test
testGroupBy = testCase "GROUP BY clause" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY clause is incorrect"
            "SELECT \"age\" FROM \"People\" GROUP BY \"age\""
            (S.codeGen selectGroupBy)

testGroupBySum :: Test
testGroupBySum = testCase "GROUP BY with SUM" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY with SUM is incorrect"
            (  "SELECT \"lastName\", SUM(\"age\") FROM \"People\" "
            <> "GROUP BY \"lastName\""
            )
            (S.codeGen groupBySum)

testGroupByAlias :: Test
testGroupByAlias = testCase "GROUP BY with an alias" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY with an alias is incorrect"
            (  "SELECT \"lastName\" AS \"name\" FROM \"People\" "
            <> "GROUP BY \"name\""
            )
            (S.codeGen groupByAlias)

testGroupByComplex :: Test
testGroupByComplex = testCase "Complex GROUP BY" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "Complex GROUP BY is invalid"
            (  "SELECT \"personId\", \"P\".\"lastName\" AS \"name\", "
            <> "SUM(\"C\".\"size\") * \"P\".\"age\" AS \"weirdFigure\" "
            <> "FROM \"People\" AS \"P\" LEFT JOIN \"Countries\" AS \"C\" "
            <> "USING (\"personId\") GROUP BY \"personId\", \"name\""
            )
            (S.codeGen groupByComplex)

testGroupBySumHaving :: Test
testGroupBySumHaving = testCase "GROUP BY with SUM and HAVING" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY with SUM and HAVING is incorrect"
            (  "SELECT \"lastName\", SUM(\"age\") "
            <> "FROM \"People\" GROUP BY \"lastName\" HAVING SUM(\"age\") > 18"
            )
            (S.codeGen groupBySumHaving)

testHavingComplex :: Test
testHavingComplex = testCase "Complex HAVING" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "Complex HAVING is invalid"
            (  "SELECT \"personId\", \"P\".\"name\", "
            <> "SUM(\"C\".\"size\" * (\"P\".\"age\" - 2)) AS \"weird\" "
            <> "FROM \"People\" AS \"P\" LEFT JOIN \"Countries\" AS \"C\" "
            <> "USING (\"personId\") WHERE \"personId\" > 2 "
            <> "GROUP BY \"personId\", \"P\".\"name\", \"P\".\"age\" "
            <> "HAVING SUM(\"P\".\"age\" * \"C\".\"size\") > 5000000"
            )
            (S.codeGen havingComplex)

----------------------------------------
-- Combined queries
----------------------------------------

testUnion :: Test
testUnion = testCase "SELECT UNION" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "SELECT UNION is incorrect"
            (  "SELECT * FROM \"People\" WHERE \"personId\" = 1 "
            <> "UNION SELECT * FROM \"People\" WHERE \"personId\" = 2"
            )
            (S.codeGen unionQuery)

testUnionCombined :: Test
testUnionCombined = testCase "Combined SELECT UNIONs" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "Combined SELECT UNIONs are incorrect"
            (  "(SELECT * FROM \"People\" WHERE \"personId\" = 1 "
            <> "UNION SELECT * FROM \"People\" WHERE \"personId\" = 2) "
            <> "INTERSECT SELECT * FROM \"People\" WHERE \"personId\" = 1"
            )
            (S.codeGen unionCombined)

testUnionAll :: Test
testUnionAll = testCase "SELECT UNION ALL" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "SELECT UNION ALL is incorrect"
            (  "SELECT * FROM \"People\" WHERE \"personId\" = 1 "
            <> "UNION ALL SELECT * FROM \"People\" WHERE \"personId\" = 2"
            )
            (S.codeGen unionAllQuery)

testIntersectAll :: Test
testIntersectAll = testCase "SELECT INTERSECT ALL" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "SELECT INTERSECT ALL is incorrect"
            (  "SELECT * FROM \"People\" WHERE \"personId\" = 1 "
            <> "INTERSECT ALL SELECT * FROM \"People\" WHERE \"personId\" = 2"
            )
            (S.codeGen intersectAllQuery)

testExcept :: Test
testExcept = testCase "SELECT EXCEPT" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "SELECT EXCEPT is incorrect"
            (  "SELECT * FROM \"People\" "
            <> "EXCEPT SELECT * FROM \"People\" WHERE \"personId\" = 1"
            )
            (S.codeGen exceptQuery)

testExceptAll :: Test
testExceptAll = testCase "SELECT EXCEPT ALL" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "SELECT EXCEPT ALL is incorrect"
            (  "SELECT * FROM \"People\" "
            <> "EXCEPT ALL SELECT * FROM \"People\" WHERE \"personId\" = 1"
            )
            (S.codeGen exceptAllQuery)

----------------------------------------
-- Comparison operators
----------------------------------------

testGreaterThan :: Test
testGreaterThan = testCase "SELECT greater than" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT greater than is incorrect"
            "SELECT * FROM \"People\" WHERE \"age\" > 18"
            (S.codeGen selectGreaterThan)

testGreaterThanOrEqualTo :: Test
testGreaterThanOrEqualTo = testCase "SELECT greater or equal to" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT greater or equal to is incorrect"
            "SELECT * FROM \"People\" WHERE \"age\" >= 18"
            (S.codeGen selectGreaterThanOrEqualTo)

testSmallerThan :: Test
testSmallerThan = testCase "SELECT smaller than" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT smaller than is incorrect"
            "SELECT * FROM \"People\" WHERE \"age\" < 18"
            (S.codeGen selectSmallerThan)

testSmallerThanOrEqualTo :: Test
testSmallerThanOrEqualTo = testCase "SELECT smaller or equal to" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT smaller or equal to is incorrect"
            "SELECT * FROM \"People\" WHERE \"age\" <= 18"
            (S.codeGen selectSmallerThanOrEqualTo)

testEqualTo :: Test
testEqualTo = testCase "SELECT equal to" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT equal to is incorrect"
            "SELECT * FROM \"People\" WHERE \"age\" = 18"
            (S.codeGen selectEqualTo)

testNotEqualTo :: Test
testNotEqualTo = testCase "SELECT not equal to" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT not equal to is incorrect"
            "SELECT * FROM \"People\" WHERE \"age\" <> 18"
            (S.codeGen selectNotEqualTo)


testNotBetween :: Test
testNotBetween = testCase "SELECT not between" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT not between is incorrect"
            "SELECT * FROM \"People\" WHERE \"age\" NOT BETWEEN 5 AND 18"
            (S.codeGen selectNotBetween)

----------------------------------------
-- Boolean operators
----------------------------------------

testIsNull :: Test
testIsNull = testCase "SELECT is null" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT is null is incorrect"
            "SELECT * FROM \"People\" WHERE \"passeportNumber\" IS NULL"
            (S.codeGen isNullQuery)

testIsNotNull :: Test
testIsNotNull = testCase "SELECT is not null" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT is not null is incorrect"
            "SELECT * FROM \"People\" WHERE \"passeportNumber\" IS NOT NULL"
            (S.codeGen isNotNullQuery)

testIsDistinctFrom :: Test
testIsDistinctFrom = testCase "SELECT is distinct from" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT is distinct from is incorrect"
            ( "SELECT * "
            <> "FROM \"People\" "
            <> "WHERE \"nickNameAsKind\" IS DISTINCT FROM \"nickNameAsAdult\""
            )
            (S.codeGen isDistinctFromQuery)


----------------------------------------
-- PostgreSQL
----------------------------------------

testSelectDistinctOnPostgreSQL :: Test
testSelectDistinctOnPostgreSQL = testCase "Select distinct on" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select distinct on query is incorrect"
           ("SELECT DISTINCT ON (\"firstName\") * "
         <> "FROM \"People\" ORDER BY \"age\"")
            (P.codeGen distinctOnSelect)

testFromLateralPostgreSQL :: Test
testFromLateralPostgreSQL = testCase "Lateral join" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Lateral join is incorrect"
            (  "SELECT * "
            <> "FROM \"Countries\", LATERAL ("
            <> "SELECT * FROM \"People\" "
            <> "WHERE \"People\".\"countryId\" = \"Countries\".\"countryId\") "
            <> "AS \"C\""
            )
            (P.codeGen fromLateral)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Select"
    [ testGroup "All vendors"
        [ testSelectAllSqLite
        , testSelectDistinctSqLite
        , testSelectTuple
        , testSelect3Tuple
        , testSelect4Tuple
        , testAdditionSqLite
        , testCurrentDateSqLite
        , testMultiplicationSqLite
        , testCrossJoinSqLite
        , testInnerJoinOnSqLite
        , testInnerJoinUsingSqLite
        , testNaturalInnerJoin
        , testLeftJoinOn
        , testLeftJoinUsing
        , testRightJoinOn
        , testFullJoinOn
        , testLeftJoinOnAnd
        , testSelfJoin
        , testCrossJoinAlias
        , testCrossRefAlias
        , testSubQuery
        , testNestedJoins
        , testWhereAlias
        , testWhereAnd
        , testWhereInValues
        , testWhereInSelect
        , testWhereBetween
        , testWhereExists
        , testOrderBy
        , testOrderByAlias
        , testOrderByAscDesc
        , testOrderByNull
        , testOrderByLimit
        , testGroupBy
        , testGroupBySum
        , testGroupByAlias
        , testGroupByComplex
        , testGroupBySumHaving
        , testHavingComplex
        , testUnion
        , testUnionCombined
        , testUnionAll
        , testIntersectAll
        , testExcept
        , testExceptAll
        , testRandomSqLite
        , testTrim
        , testGreaterThan
        , testGreaterThanOrEqualTo
        , testSmallerThan
        , testSmallerThanOrEqualTo
        , testEqualTo
        , testNotEqualTo
        , testNotBetween
        , testIsNull
        , testIsNotNull
        , testIsDistinctFrom
        ]
    , testGroup "PostgreSQL"
        [ testLastInsertIdPostgreSQL
        , testSelectDistinctOnPostgreSQL
        , testFromLateralPostgreSQL
        ]
    , testGroup "MariaDb"
        [ testLastInsertIdMariaDB
        ]
    , testGroup "SqLite"
        [ testLastInsertIdSqLite
        ]
    ]
