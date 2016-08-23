{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Database/Hedsql/Statements/Query.hs
Description : Collection of SELECT queries.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of SELECT queries to be used in tests or as examples.
-}
module Database.Hedsql.Statements.Select
    ( -- * All DB vendors

      -- ** SELECT
      selectAll
    , selectTwoCols
    , selectTuple
    , select3Tuple
    , distinctSelect

      -- ** FROM

      -- *** Joins
    , fromCrossJoin
    , fromInnerJoinOn
    , fromInnerJoinUsing
    , fromNaturalInnerJoin
    , fromLeftJoinOn
    , fromLeftJoinUsing
    , fromRightJoinOn
    , fromFullJoinOn
    , fromLeftJoinOnAnd
    , selfJoin
    , crossJoinAlias
    , crossRefAlias
    , nestedJoins

      -- *** Sub-queries
    , selectSubQuery

      -- ** WHERE
    , selectGen
    , whereAlias
    , whereAnd
    , whereAnds
    , whereInValues
    , whereInSelect
    , whereBetween
    , whereExists

      -- ** ORDER BY
    , orderByQuery
    , orderBySum
    , orderByAscDesc
    , Database.Hedsql.Statements.Select.orderByLimit
    , orderByNull
    , orderByLimitOffset

      -- ** GROUP BY
    , Database.Hedsql.Statements.Select.selectGroupBy
    , groupByTwo
    , groupBySum
    , groupByAlias
    , groupByComplex
    , groupBySumHaving
    , groupBySumHavingTwo
    , havingComplex

      -- ** Full
    , selectFull

      -- ** Functions
    , addition
    , multiplication
    , selectCurrentDate
    , selectRandom
    , selectLastInsertId
    , selectTrim

      -- ** Combined queries
    , unionQuery
    , unionCombined
    , unionAllQuery
    , intersectAllQuery
    , exceptQuery
    , exceptAllQuery

    -- * PostgreSQL
    , distinctOnSelect
    , fromLateral
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql
import Database.Hedsql.Ext
import Database.Hedsql.SqLite

import qualified Database.Hedsql.Drivers.PostgreSQL.Constructor as P
import qualified Database.Hedsql.PostgreSQL                     as Pg

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All Vendors
----------------------------------------

--------------------
-- SELECT
--------------------

-- | > SELECT * FROM "People"
selectAll :: Select [[Undefined]] dbVendor
selectAll =
       select (//*)
    |> from "People"
    |> end

{-|
@
SELECT
  "firstName",
  "lastName"
FROM "People"
@
-}
selectTwoCols :: Select [[Undefined]] dbVendor
selectTwoCols =
       select ["firstName", "lastName"]
    |> from "People"
    |> end

{-|
@
SELECT
  "firstName",
  "age"
FROM "People"
@
-}
selectTuple :: Select [(String, Int)] dbVendor
selectTuple =
       select (c1, c2)
    |> from "People"
    |> end
    where
       c1 = col "firstName" (varchar 256)
       c2 = col "age" integer

{-|
@
SELECT
  "firstName",
  "lastName",
  "age"
FROM "People"
@
-}
select3Tuple :: Select [(String, String, Int)] dbVendor
select3Tuple =
       select (c1, c2, c3)
    |> from "People"
    |> end
    where
       c1 = col "firstName" (varchar 256)
       c2 = col "lastName" (varchar 256)
       c3 = col "age" integer

-- | > SELECT DISTINCT "firstName" FROM "People"
distinctSelect :: Select [Undefined] dbVendor
distinctSelect =
       selectDistinct "firstName"
    |> from "People"
    |> end

--------------------
-- FROM
--------------------

-- Joins
--------------------

-- | > SELECT * FROM "People" CROSS JOIN "Countries"
fromCrossJoin :: Select [[Undefined]] dbVendor
fromCrossJoin =
       select (//*)
    |> from ("People" `crossJoin` "Countries")
    |> end

{-|
@
SELECT *
FROM "People"
INNER JOIN "Countries"
ON "People"."countryId" = "Countries"."countryId"
@
-}
fromInnerJoinOn :: Select [[Undefined]] dbVendor
fromInnerJoinOn =
       select (//*)
    |> from (innerJoin "People" "Countries" $ "People" /. "countryId" /== "Countries" /. "countryId")
    |> end

-- | > SELECT * FROM "People" INNER JOIN "Countries" USING ("country")
fromInnerJoinUsing :: Select [[Undefined]] dbVendor
fromInnerJoinUsing =
       select (//*)
    |> from (innerJoin "People" "Countries" "countryId")
    |> end

-- | > SELECT * FROM "People" NATURAL INNER JOIN "Countries"
fromNaturalInnerJoin :: Select [[Undefined]] dbVendor
fromNaturalInnerJoin =
       select (//*)
    |> from ("People" `naturalInnerJoin` "Countries")
    |> end

{-|
> SELECT * FROM "People" LEFT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromLeftJoinOn :: Select [[Undefined]] dbVendor
fromLeftJoinOn =
       select (//*)
    |> from (leftJoin "People" "Countries" $ "People"/."countryId" /== "Countries"/."countryId")
    |> end

-- | > SELECT * FROM "People" LEFT JOIN "Countries" USING ("countryId")
fromLeftJoinUsing :: Select [[Undefined]] dbVendor
fromLeftJoinUsing =
       select (//*)
    |> from (leftJoin "People" "Countries" "countryId")
    |> end

{-|
> SELECT * FROM "People" RIGHT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromRightJoinOn :: Select [[Undefined]] dbVendor
fromRightJoinOn =
       select (//*)
    |> from (rightJoin "People" "Countries" $ "People"/."countryId" /== "Countries"/."countryId")
    |> end

{-|
> SELECT * FROM "People" FULL JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromFullJoinOn :: Select [[Undefined]] dbVendor
fromFullJoinOn =
       select (//*)
    |> from (fullJoin t1 t2 $ c1 /== c2)
    |> end
    where
        t1 = table "People"
        t2 = table "Countries"
        c1 = t1/."countryId"
        c2 = t2/."countryId"

{-|
> SELECT * FROM "People" LEFT JOIN "Countries"
> ON ("People"."countryId" = "Countries"."countryId"
>      AND "Countries"."name" = 'Italy')
-}
fromLeftJoinOnAnd :: Select [[Undefined]] dbVendor
fromLeftJoinOnAnd =
       select (//*)
    |> from (leftJoin t1 t2 cond)
    |> end
    where
        t1 = table "People"
        t2 = table "Countries"
        cond = leftPart `and_` rightPart
        leftPart =  t1/."countryId" /== t2/."countryId"
        rightPart = t2/. col "name" (varchar 256) /== value "Italy"

{-|
> SELECT *
> FROM "People" AS "Father"
>   INNER JOIN "People" AS "Child" ON "Father"."personId" = "Child"."father"
-}
selfJoin :: Select [[Undefined]] dbVendor
selfJoin =
       select (//*)
    |> from (innerJoin father child cond)
    |> end
    where
        cond = (father/."personId") /== (child/."father")
        father = people `alias` "Father"
        child = people `alias` "Child"
        people = table "People"

-- | > SELECT * FROM "People" AS "P" CROSS JOIN "Countries" AS "C"
crossJoinAlias :: Select [[Undefined]] dbVendor
crossJoinAlias =
       select (//*)
    |> from (crossJoin ("People" `alias` "P") ("Countries" `alias` "C"))
    |> end

-- | > SELECT * FROM ("People" AS "P" CROSS JOIN "Countries") AS "PC";
crossRefAlias :: Select [[Undefined]] dbVendor
crossRefAlias =
       select (//*)
    |> from ((("People" `alias` "P") `crossJoin` "Countries") `alias` "PC")
    |> end

{-|
@
SELECT *
FROM "People"
INNER JOIN "Countries"
ON "People"."countryId" = "Countries"."countryId"
INNER JOIN "Addresses"
ON "People"."personId" = "Addresses"."personId"
@
-}
nestedJoins :: Select [[Undefined]] dbVendor
nestedJoins =
       select (//*)
    |> from join2
    |> end
    where
        join1 = innerJoin "People" "Countries" $ "People" /. "countryId" /== "Countries" /. "countryId"
        join2 = innerJoin join1 "Addresses" $ "People" /. "personId" /== "Addresses" /. "personId"

-- Sub-queries
--------------------

{-|
@
SELECT *
FROM (SELECT *
      FROM "People") AS "P"
@
-}
selectSubQuery :: Select [[Undefined]] dbVendor
selectSubQuery =
       select (//*)
    |> from (subQuery (
           select (//*)
        |> from "People") "P")
    |> end

--------------------
-- WHERE
--------------------

{-|
SELECT using a generic columns and values.

> SELECT "firstName" FROM "People" WHERE "age" > 18
-}
selectGen :: Select [Undefined] dbVendor
selectGen =
       select "firstName"
    |> from "People"
    |> where_ ("age" /> genVal (18::Int))
    |> end

-- | > SELECT * FROM "People" AS "P" WHERE "P"."age" > 18;
whereAlias :: Select [[Undefined]] dbVendor
whereAlias =
       select (//*)
    |> from p
    |> where_ (p/. col "age" integer /> intVal 5)
    |> end
    where
        p = table "People" `alias` "P"

{-|
@
SELECT *
FROM
  "People",
  "Countries"
WHERE
  "People"."countryId" = "Countries"."countryId"
  AND "People"."age" > 18
@
-}
whereAnd :: Select [[Undefined]] dbVendor
whereAnd =
       select (//*)
    |> from [people, countries]
    |> where_ ((people/.id' /== countries/.id')
        `and_` (people/. col "age" integer /> intVal 18))
    |> end
    where
        people = tableRef "People"
        countries = tableRef "Countries"
        id' = colRef "countryId"

{-|
@
SELECT *
FROM
  "People",
  "Countries"
WHERE
  "People"."countryId" = "Countries"."countryId"
  AND "People"."age" > 18
  AND "People"."age" < 70
@
-}
whereAnds :: Select [[Undefined]] dbVendor
whereAnds =
       select (//*)
    |> from [people, countries]
    |> where_ ((people/.id' /== countries/.id')
        `and_` (people/. age /> intVal 18)
        `and_` (people/. age /< intVal 70))
    |> end
    where
        people = tableRef "People"
        countries = tableRef "Countries"
        id' = colRef "countryId"
        age = col "age" integer

-- | > SELECT * FROM "Countries" WHERE "name" IN ('Italy', 'Switzerland')
whereInValues :: Select [[Undefined]] dbVendor
whereInValues =
       select (//*)
    |> from "Countries"
    |> where_ (col "name" (varchar 256) `in_` cs)
    |> end
    where
        cs = colRef $ map genQVal ["Italy", "Switzerland"]

{-|
@
SELECT *
FROM "People"
WHERE "countryId" IN (SELECT "countryId"
                      FROM "Countries"
                      WHERE "inhabitants" >= "size" * 100)
@
-}
whereInSelect :: Select [[Undefined]] dbVendor
whereInSelect =
       select (//*)
    |> from "People"
    |> where_ (countryId `in_` query)
    |> end
    where
        countryId = col "countryId" integer
        query =
               select countryId
            |> from "Countries"
            |> where_ (col "inhabitants" integer />= (col "size" integer /* intVal 100))
            |> end

{-|
@
SELECT *
FROM "Countries"
WHERE ("inhabitants" BETWEEN 10000 AND 10000000)
@
-}
whereBetween :: Select [[Undefined]] dbVendor
whereBetween =
       select (//*)
    |> from "Countries"
    |> where_ (between (col "inhabitants" integer) (intVal 10000) (intVal 1000000))
    |> end

{-|
@
SELECT *
FROM "People"
WHERE EXISTS (
    SELECT *
    FROM "Countries"
    WHERE "People"."countryId" = "Countries"."countryId")
@
-}
whereExists :: Select [[Undefined]] dbVendor
whereExists =
       select (//*)
    |> from "People"
    |> where_ (exists query)
    |> end
    where
        query =
               select (//*)
            |> from "Countries"
            |> where_ ("People"/."countryId" /== "Countries"/."countryId")
            |> end

--------------------
-- ORDER BY
--------------------

{-|
> SELECT "firstName" FROM "People" ORDER BY "firstName
-}
orderByQuery :: Select [String] dbVendor
orderByQuery =
       select c
    |> from "People"
    |> orderBy c
    |> end
    where
        c = col "firstName" (varchar 256)

{-|
> SELECT "size" + "inhabitants" AS "sum", "name"
> FROM "Countries"
> ORDER BY "sum"
-}
orderBySum :: Select [[Undefined]] dbVendor
orderBySum =
       select [sum', colRefWrap "name"]
    |> from "Countries"
    |> orderBy sum'
    |> end
    where
        sum' =
            wrap $ (col "size" integer /+ col "inhabitants" integer) `as_` "sum"

{-|
@
SELECT
  "firstName",
  "lastName"
FROM "People"
ORDER BY
  "firstName" ASC,
  "lastName" DESC
@
-}
orderByAscDesc :: Select [[String]] dbVendor
orderByAscDesc =
       select [firstName, lastName]
    |> from "People"
    |> orderBy [asc firstName, desc lastName]
    |> end
    where
        firstName = col "firstName" (varchar 256)
        lastName = col "lastName" (varchar 256)

{-|
> SELECT "age", "passeportNumber"
> FROM "People"
> ORDER BY "age" NULLS FIRST, "passeportNumber" NULLS LAST"
-}
orderByNull :: Select [[Int]] dbVendor
orderByNull =
       select [age, passeport]
    |> from "People"
    |> orderBy [nullsFirst age, nullsLast passeport]
    |> end
    where
        age = col "age" integer
        passeport = col "passeportNumber" integer

{-|
@
SELECT *
FROM "People"
ORDER BY "firstName"
LIMIT 2
@
-}
orderByLimit :: Select [[Undefined]] dbVendor
orderByLimit =
       select (//*)
    |> from "People"
    |> orderBy "firstName"
    |> limit 2
    |> end

{-|
@
SELECT *
FROM "People"
ORDER BY "firstName"
LIMIT 5 OFFSET 2
@
-}
orderByLimitOffset :: Select [[Undefined]] dbVendor
orderByLimitOffset =
       select (//*)
    |> from "People"
    |> orderBy "firstName"
    |> limit 5
    |> offset 2
    |> end

--------------------
-- GROUP BY
--------------------

{-|
@
SELECT "age"
FROM "People"
GROUP BY "age"
@
-}
selectGroupBy :: Select [Int] dbVendor
selectGroupBy =
       select age
    |> from "People"
    |> groupBy age
    |> end
    where
        age = col "age" integer

{-|
@
SELECT
  "firstName",
  "age"
FROM "People"
GROUP BY
  "firstName",
  "age"
@
-}
groupByTwo :: Select [[Undefined]] dbVendor
groupByTwo =
       select cs
    |> from "People"
    |> groupBy cs
    |> end
    where
        cs = ["firstName", "age"]

-- | > SELECT "lastName", sum("age") FROM "People" GROUP BY "lastName";
groupBySum :: Select [[Undefined]] dbVendor
groupBySum =
       select [lastName, colRefWrap $ sum_ $ col "age" integer]
    |> from "People"
    |> groupBy lastName
    |> end
    where
        lastName = colRefWrap $ col "lastName" (varchar 256)

-- | > SELECT "lastName" AS "name" FROM "People" GROUP BY "name"
groupByAlias :: Select [String] dbVendor
groupByAlias =
       select name
    |> from "People"
    |> groupBy name
    |> end
    where
        name = col "lastName" (varchar 256) `as_` "name"

{-|
@
SELECT
    "personId",
    "P"."lastName" AS "name",
    SUM("C"."size") * "P"."age" AS "weirdFigure"
FROM "People" AS "P" LEFT JOIN "Countries" AS "C" USING ("personId")
GROUP BY "personId", "name"
@
-}
groupByComplex :: Select [[Undefined]] dbVendor
groupByComplex =
        select [colRefWrap personId, name, weird]
     |> from (leftJoin people countries personId)
     |> groupBy [colRefWrap personId, name]
     |> end
     where
         name = colRefWrap $ (people/."lastName") `as_` "name"
         personId = toCol "personId"
         age = people /. col "age" integer
         weird = colRefWrap $ (sum_ (countries /. col "size" integer) /* age)
                    `as_` "weirdFigure"
         people = table "People" `alias` "P"
         countries = table "Countries" `alias` "C"

{-|
@
SELECT
  "lastName",
  SUM("age")
FROM "People"
GROUP BY "lastName"
HAVING SUM("age") > 18
@
-}
groupBySumHaving :: Select [[Undefined]] dbVendor
groupBySumHaving =
       select [lastName, colRefWrap sumAge]
    |> from "People"
    |> groupBy lastName
    |> having (sumAge /> intVal 18)
    |> end
    where
         lastName = colRefWrap "lastName"
         sumAge = sum_ $ col "age" integer

{-|
@
SELECT "firstName"
FROM "People"
GROUP BY "firstName"
HAVING
  sum("age") > 18
  OR sum("size") < 1800
@
-}
groupBySumHavingTwo :: Select [Undefined] dbVendor
groupBySumHavingTwo =
       select "firstName"
    |> from "People"
    |> groupBy "firstName"
    |> having ((sumAge /> intVal 18) `or_` (sumSize /< intVal 1800))
    |> end
    where
        sumAge = sum_ $ col "age" integer
        sumSize = sum_ $ col "size" integer

{-|
@
SELECT "personId", "P"."name", SUM("C"."size" * ("P"."age" - 2)) AS "weird"
FROM "People" AS "P" LEFT JOIN "Countries" AS "C" USING ("personId")
WHERE "personId" > 2
GROUP BY "personId", "P"."name", "P"."age"
HAVING SUM("P"."age" * "C"."size") > 5000000
@
-}
havingComplex :: Select [[Undefined]] dbVendor
havingComplex =
        select [colRefWrap personId, colRefWrap name, wrap weird]
     |> from (leftJoin people countries personId)
     |> where_ (personId /> intVal 2)
     |> groupBy [colRefWrap personId, wrap name, wrap age]
     |> having (sum_ (age /* size) /> intVal 5000000)
     |> end
     where
         name      = people/."name"
         personId  = col "personId" integer
         age       = people /. col "age" integer
         size      = countries /. col "size" integer
         people    = table "People" `alias` "P"
         countries = table "Countries" `alias`"C"
         weird     = sum_ (size /* (age /- intVal 2)) `as_` "weird"

--------------------
-- Full
--------------------

{-|
@
SELECT *
FROM "People"
WHERE "age" > 18
GROUPBY "lastName"
HAVING SUM("age") > 100
ORDER BY "id"
LIMIT 30 OFFSET 2
@
-}
selectFull :: Select [[Undefined]] dbVendor
selectFull =
       select (//*)
    |> from (table "People")
    |> where_ (age /> intVal 18)
    |> groupBy (col "lastName" $ varchar 256)
    |> having (sum_ age /> intVal 100)
    |> orderBy (col "id" integer)
    |> limit 30
    |> offset 2
    |> end
    where
        age = col "age" integer

--------------------
-- Comparison operators
--------------------

-- | Query all rows from the People table.
selectPeople :: SelectFromStmt [[Undefined]] dbVendor
selectPeople =
       select (//*)
    |> from "People"

-- | > SELECT * FROM "People" WHERE "age" > 18
selectGreaterThan :: Select [[Undefined]] dbVendor
selectGreaterThan =
       selectPeople
    |> where_ (col "age" integer /> intVal 18)
    |> end

-- | > SELECT * FROM "People" WHERE "age" >= 18
selectGreaterThanOrEqualTo :: Select [[Undefined]] dbVendor
selectGreaterThanOrEqualTo =
       selectPeople
    |> where_ (col "age" integer />= intVal 18)
    |> end

-- | > SELECT * FROM "People" WHERE "age" < 18
selectSmallerThan :: Select [[Undefined]] dbVendor
selectSmallerThan =
       selectPeople
    |> where_ (col "age" integer /< intVal 18)
    |> end

-- | > SELECT * FROM "People" WHERE "age" <= 18
selectSmallerThanOrEqualTo :: Select [[Undefined]] dbVendor
selectSmallerThanOrEqualTo =
       selectPeople
    |> where_ (col "age" integer /<= intVal 18)
    |> end

-- | > SELECT * FROM "People" WHERE "age" = 18
selectEqual :: Select [[Undefined]] dbVendor
selectEqual =
       selectPeople
    |> where_ (col "age" integer /== intVal 18)
    |> end

-- | > SELECT * FROM "People" WHERE "age" <> 18
selectNotEqual :: Select [[Undefined]] dbVendor
selectNotEqual =
       selectPeople
    |> where_ (col "age" integer /<> intVal 18)
    |> end

-- | > SELECT * FROM "People" WHERE ("age" NOT BETWEEN 5 AND 18)
selectNotBetween :: Select [[Undefined]] dbVendor
selectNotBetween =
       selectPeople
    |> where_ (notBetween (col "age" integer) (intVal 5) $ intVal 18)
    |> end

--------------------
-- Boolean operators
--------------------

-- | > SELECT * FROM "People" WHERE "passeportNumber" IS NULL
isNullQuery :: Select [[Undefined]] dbVendor
isNullQuery =
       selectPeople
    |> where_ (isNull "passeportNumber")
    |> end

-- | > SELECT * FROM "People" WHERE "passeportNumber" IS NOT NULL
isNotNullQuery :: Select [[Undefined]] dbVendor
isNotNullQuery =
       selectPeople
    |> where_ (isNotNull "passeportNumber")
    |> end

{-|
> SELECT *
> FROM "People" WHERE "nickNameAsKind" IS DISTINCT FROM "nickNameAsAdult"
-}
isDistinctFromQuery :: Select [[Undefined]] dbVendor
isDistinctFromQuery =
       selectPeople
    |> where_ ("nickNameAsKind" `isDistinctFrom` "nickNameAsAdult")
    |> end

{-|
> SELECT *
> FROM "People" WHERE "nickNameAsKind" IS NOT DISTINCT FROM "nickNameAsAdult"
-}
isNotDistinctFromQuery :: Select [[Undefined]] dbVendor
isNotDistinctFromQuery =
       selectPeople
    |> where_ ("nickNameAsKind" `isNotDistinctFrom` "nickNameAsAdult")
    |> end

-- | > SELECT * FROM "People" WHERE "married" IS TRUE
isTrueQuery :: Select [[Undefined]] dbVendor
isTrueQuery =
       selectPeople
    |> where_ (isTrue $ col "married" boolean)
    |> end

-- | > SELECT * FROM "People" WHERE "married" IS NOT TRUE
isNotTrueQuery :: Select [[Undefined]] dbVendor
isNotTrueQuery =
       selectPeople
    |> where_ (isNotTrue $ col "married" boolean)
    |> end

-- | > SELECT * FROM "People" WHERE "married" IS FALSE
isFalseQuery :: Select [[Undefined]] dbVendor
isFalseQuery =
       selectPeople
    |> where_ (isFalse $ col "married" boolean)
    |> end

-- | > SELECT * FROM "People" WHERE "married" IS NOT FALSE
isNotFalseQuery :: Select [[Undefined]] dbVendor
isNotFalseQuery =
       selectPeople
    |> where_ (isNotFalse $ col "married" boolean)
    |> end

{-|
> SELECT * FROM "People"
> WHERE ("nickNameAsKind" = "nickNameAsAdult") IS UNKWNOWN
-}
isUnknownQuery :: Select [[Undefined]] dbVendor
isUnknownQuery =
       selectPeople
    |> where_ (isUnknown $ "nickNameAsKind" /== "nickNameAsAdult")
    |> end

{-|
> SELECT *
> FROM "People" WHERE ("nickNameAsKind" = "nickNameAsAdult") IS NOT UNKWNOWN
-}
isNotUnknownQuery :: Select [[Undefined]] dbVendor
isNotUnknownQuery =
       selectPeople
    |> where_ (isNotUnknown $ "nickNameAsKind" /== "nickNameAsAdult")
    |> end

--------------------
-- Functions
--------------------

-- | > SELECT "age" + 1 FROM "People"
addition :: Select [Int] dbVendor
addition =
       select (col "age" integer /+ intVal 1)
    |> from "People"
    |> end

-- | > SELECT 3 * 4
multiplication :: Select [Int] dbVendor
multiplication =
       select (intVal 3 /* intVal 4)
    |> end

{-|
MariaDB & PostgreSQL
> SELECT CURRENT_DATE

SqLite
> SELECT Date('now')
-}
selectCurrentDate :: Select [Time] dbVendor
selectCurrentDate =
       select currentDate
    |> end

{-|
MariaDB
> SELECT RAND()

PostgreSQL & SqLite
> SELECT random()
-}
selectRandom :: Select [Int] dbVendor
selectRandom =
       select random
    |> end

{-|
> SELECT TRIM("name")
-}
selectTrim :: SelectSingleStmt [String] dbVendor
selectTrim =
  select $ trim name
  where
    name = col "name" $ varchar 256

{-|
PostgreSQL:
> SELECT LASTVAL()

MariaDB:
> SELECT LAST_INSERT_ID()

SQLite:
> SELECT last_insert_row_id()
-}
selectLastInsertId :: Select [Int] dbVendor
selectLastInsertId =
       select lastInsertId
    |> end

--------------------
-- Combined queries
--------------------

-- | Query a person by its primary key.
selectId :: Int -> Select [[Undefined]] dbVendor
selectId id' =
       select (//*)
    |> from "People"
    |> where_ (col "personId" integer /== value id')
    |> end

{-|
@
SELECT *
FROM "People"
WHERE "personId" = 1
UNION
SELECT *
FROM "People"
WHERE "personId" = 2
@
-}
unionQuery :: Select [[Undefined]] dbVendor
unionQuery = union (selectId 1) $ selectId 2

{-|
> (SELECT * FROM "People" WHERE "personId" = 1
> UNION
> SELECT * FROM "People" WHERE "personId" = 2)
> INTERSECT
> SELECT * FROM "People" WHERE "personId" = 1
-}
unionCombined :: Select [[Undefined]] dbVendor
unionCombined =
    intersect
        unionQuery (
               select (//*)
            |> from "People"
            |> where_ (col "personId" integer /== intVal 1)
            |> end)

{-|
> SELECT * FROM "People" WHERE "personId" = 1
> UNION ALL
> SELECT * FROM "People" WHERE "personId" = 2
-}
unionAllQuery :: Select [[Undefined]] dbVendor
unionAllQuery = unionAll (selectId 1) $ selectId 2

{-|
> SELECT * FROM "People" WHERE "personId" = 1
> INTERSECT ALL
> SELECT * FROM "People" WHERE "personId" = 2
-}
intersectAllQuery :: Select [[Undefined]] dbVendor
intersectAllQuery = intersectAll (selectId 1) $ selectId 2

{-|
> SELECT * FROM "People"
> EXCEPT
> SELECT * FROM "People" WHERE "personId" = 1
-}
exceptQuery :: Select [[Undefined]] dbVendor
exceptQuery = except (
       select (//*)
    |> from "People"
    |> end) (selectId 1)

{-|
> SELECT * FROM "People"
> EXCEPT ALL
> SELECT * FROM "People" WHERE "personId" = 1
-}
exceptAllQuery :: Select [[Undefined]] dbVendor
exceptAllQuery =
    exceptAll (
           select (//*)
        |> from "People"
        |> end) (selectId 1)

----------------------------------------
-- PostgreSQL
----------------------------------------

-- | > SELECT DISTINCT ON ("firstName") * FROM "People" ORDER BY "age"
distinctOnSelect :: Select [[Undefined]] Pg.PostgreSQL
distinctOnSelect =
       P.selectDistinctOn [colRefWrap "firstName"] (//*)
    |> from "People"
    |> orderBy "age"
    |> end

{-|
SELECT * FROM "Countries", LATERAL (
    SELECT *
    FROM "People"
    WHERE "People"."countryId" = "Countries"."countryId") AS "C"
-}
fromLateral :: Select [[Undefined]] Pg.PostgreSQL
fromLateral =
       select (//*)
    |> from [tableRef "Countries", P.lateral (wrap sQuery) "C"]
    |> end
    where
        sQuery =
               select (//*)
            |> from "People"
            |> where_ ("People"/."countryId" /== "Countries"/."countryId")
            |> end

----------------------------------------
-- Quick start tutorial
----------------------------------------

myQueryOne :: Select [[Undefined]] dbVendor
myQueryOne =
       select (//*)
    |> from (table "films")
    |> end

myQueryTwo :: Select [[Int]] SqLite
myQueryTwo =
       select [id', age]
    |> from films
    |> where_ (age /+ intVal 20 `in_` subSelect)
    |> end
    where
        id' = col "id" integer
        age = col "age" integer
        films = table "films"
        actor = table "actors"
        subSelect =
               select age
            |> from actor
            |> end

myQueryThree :: Select [[Undefined]] Pg.PostgreSQL
myQueryThree =
           select (//*)
        |> from [tableRef $ table "foo", Pg.lateral (wrap sub) "s"]
        |> end
        where
            sub =
                   select (//*)
                |> from "bar"
                |> end

myQueryFour :: Select [[Undefined]] dbVendor
myQueryFour =
       select ["firstName", "lastName"]
    |> from "People"
    |> where_ ("age" /> genVal (18::Int))
    |> end

myQueryFive :: Select [[Undefined]] dbVendor
myQueryFive =
       select [wrap name, wrap age]
    |> from (table "People")
    |> end
    where
        name = col "name" $ varchar 256
        age = col "age" integer
