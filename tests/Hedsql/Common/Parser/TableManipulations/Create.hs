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

import Hedsql.SqLite

import qualified Hedsql.PostgreSQL                     as Pg
import qualified Hedsql.Drivers.PostgreSQL.Constructor as P

-- | > CREATE TABLE "People" ("firstName" varchar(256))        
simpleTable :: CreateTable a
simpleTable = createTable "People" [column "firstName" /++ varchar 256]

{-|
Maria DB and SqLite:
> CREATE TABLE "People" ("id" INTEGER PRIMARY KEY AUTOINCREMENT)

PostgreSQL:
> CREATE TABLE "People" ("id" serial PRIMARY KEY)
-}
primaryKeyCol :: CreateTable a
primaryKeyCol = createTable "People" [column "id" /++ integer /++ primary True]


{-|
CREATE TABLE "People" (
    "firstName"       varchar(256),
    "lastName"        varchar(256),
    CONSTRAINT "pk" PRIMARY KEY ("firstName", "lastName")
)
-}
primaryKeyTable :: CreateTable a
primaryKeyTable =
    createTable
        "People"
        [column "firstName" /++ varchar 256, column "lastName" /++ varchar 256]
        /++ (tableConstraint "pk" $ primaryT ["firstName", "lastName"]) 
     
-- | CREATE TABLE "People" ("age" integer CHECK ("age" > -1))
createCheck :: CreateTable a
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
createChecks :: CreateTable a
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
