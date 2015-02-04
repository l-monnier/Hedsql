{-|
Module      : Hedsql/Common/Parser/TableManipulations/Create.hs
Description : Collection of CREATE statements.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of CREATE statements to be used in tests or as examples.
-}
module Hedsql.Common.Parser.TableManipulations.Create where

import Database.Hedsql.SqLite
      
-- | CREATE TABLE "People" ("country" varchar(256) DEFAULT('Switzerland'))
defaultVal :: Table a
defaultVal = createTable
    "People"
    [column "country" /++ varchar 256 /++ defaultValue (value "Switzerland")]

{-|
CREATE TABLE "People" (
    "firstName" varchar(256) CONSTRAINT "no_null" NOT NULL,
    "lastName"  varchar(256) NOT NULL
)
-}
noNulls :: Table a
noNulls =
    createTable "People" cols
    where
        cols =
            [ column "firstName" /++ varchar 256 /++ colConstraint "no_null" notNull
            , column "lastName"  /++ varchar 256 /++ notNull
            ]

-- | > CREATE TABLE "People" ("firstName" varchar(256))        
simpleTable :: Table a
simpleTable = createTable "People" [column "firstName" /++ varchar 256]

{-|
Maria DB and SqLite:
> CREATE TABLE "People" ("id" INTEGER PRIMARY KEY)

PostgreSQL:
> CREATE TABLE "People" ("id" integer PRIMARY KEY)
-}
primaryKeyCol :: Table a
primaryKeyCol =
    createTable "People" [column "id" /++ integer /++ primary False]

{-|
Maria DB and SqLite:
> CREATE TABLE "People" ("id" INTEGER PRIMARY KEY AUTOINCREMENT)

PostgreSQL:
> CREATE TABLE "People" ("id" serial PRIMARY KEY)
-}
primaryKeyColAuto :: Table a
primaryKeyColAuto =
    createTable "People" [column "id" /++ integer /++ primary True]

{-|
CREATE TABLE "People" (
    "firstName"       varchar(256),
    "lastName"        varchar(256),
    CONSTRAINT "pk" PRIMARY KEY ("firstName", "lastName")
)
-}
primaryKeyTable :: Table a
primaryKeyTable =
    createTable
        "People"
        [column "firstName" /++ varchar 256, column "lastName" /++ varchar 256]
        /++ tableConstraint "pk" (primaryT ["firstName", "lastName"])
     
-- | CREATE TABLE "People" ("age" integer CHECK ("age" > -1))
createCheck :: Table a
createCheck =
    createTable
        "People"
        [column "age"  /++ integer /++ check ("age" /> (-1::Int))]
        
{-|            
CREATE TABLE "People" (
    "lastName" varchar(256),
    "age"      integer,
    CONSTRAINT "checks" CHECK ("age" > -1 AND "lastName" <> '')
)
-}
createChecks :: Table a
createChecks =
    createTable
        "People"
        [ column "lastName" /++ varchar 256
        , column "age"      /++ integer
        ] /++ c1
    where
        c1 =
            tableConstraint "checks" $
                  checkT $ "age" /> (-1::Int)
            `and_`("lastName"    /<> value "")

{-|
CREATE TABLE "People" ("countryId" integer REFERENCES "Countries"("countryId"))
-}
createFK :: Table a
createFK =
    createTable
        "People"
        [column "countryId" /++ integer /++ foreignKey "Countries" "countryId"]

-- | CREATE TABLE "People" ("passportNo" varchar(256) UNIQUE)
createUnique :: Table a
createUnique =
    createTable "People" [column "passportNo" /++ varchar 256 /++ unique]
    
{-|
CREATE TABLE "People" (
    "firstName" varchar(256),
    "lastName"  varchar(256),
    UNIQUE ("firstName", "lastName")
)
-}
createUniqueT :: Table a
createUniqueT =
    createTable "People" cols /++ tableConstraint "" (uniqueT cols)
    where
        cols =
            [ column "firstName" /++ varchar 256
            , column "lastName"  /++ varchar 256
            ]