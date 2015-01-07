{-# LANGUAGE NoOverloadedStrings #-}

{-|
Module      : Statements/Create.hs
Description : CREATE TABLE statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

CREATE TABLE statements used in the test suite.
-}
module Statements.Create where

import Hedsql.Default

import Prelude hiding (and)

{-|
CREATE TABLE "films" (
    "code"        char(5),
    "title"       varchar(40),
    "did"         integer,
    "date_prod"   date,
    "kind"        varchar(10)
)
-}        
createFilms :: CreateTable
createFilms = 
    createTable "films" cols
    where
        cols = [
              column "code" /++ char 5
            , column "title" /++ varchar 40
            , column "did" /++ integer
            , column "date_prod" /++ date
            , column "kind" /++ varchar 10
            ]
        --constraints = tableConstraint "" $ uniqueT ["date_prod"]

{-|       
CREATE TABLE "films" (
    "code"        char(5),
    "title"       varchar(40),
    "did"         integer CHECK ("did" > 100),
    "date_prod"   date,
    "kind"        varchar(10)
)
-}
createCheck :: CreateTable
createCheck =
    createTable "films" cols
    where
        cols = [
              column "code" /++ char 5
            , column "title" /++ varchar 40
            , column "did" /++ integer /++ checkConstraint
            , column "date_prod" /++ date
            , column "kind" /++ varchar 10
            ]
        checkConstraint =
            colConstraint "did check" $ check ("did" /> (100::Int))

{-|            
CREATE TABLE "films" (
    "code"        char(5),
    "title"       varchar(40),
    "did"         integer,
    "date_prod"   date,
    "kind"        varchar(10)
    CONSTRAINT "con1" CHECK ("did" > 100 AND "title" <> '')
)
-}
createMultiChecks :: CreateTable
createMultiChecks =
    createFilms /++ con1
    where
        con1 =
            tableConstraint "multi-checks" $ checkT $ "did" /> (100::Int)
            `and`("title" /<> toValue "")

{-|
CREATE TABLE "films" (
    "code"        char(5),
    "title"       varchar(40),
    "did"         integer,
    "date_prod"   date,
    "kind"        varchar(10),
    CONSTRAINT code_title PRIMARY KEY("code", "title")
)
-}
createPrimaryKey :: CreateTable
createPrimaryKey =
    createTable "films" cols /++ constraints
    where
        cols = [
              column "code" /++ char 5
            , column "title" /++ varchar 40
            , column "did" /++ integer
            , column "date_prod" /++ date
            , column "kind" /++ varchar 10
            ]
        constraints = tableConstraint "code_title" $ primaryT ["code", "title"]

{-|        
CREATE TABLE "films" (
    "code"        char(5) PRIMARY KEY,
    "title"       varchar(40),
    "did"         integer,
    "date_prod"   date,
    "kind"        varchar(10)
) 
-}
createPrimaryKeyCol :: CreateTable
createPrimaryKeyCol =
    createTable "films" cols
    where
        cols = [
              column "code" /++ char 5 /++ primary False
            , column "title" /++ varchar 40
            , column "did" /++ integer
            , column "date_prod" /++ date
            , column "kind" /++ varchar 10
            ]

{-|        
CREATE TABLE "films" (
    "code"        char(5),
    "title"       varchar(40) DEFAULT 'Luso Films',
    "did"         integer,
    "date_prod"   date,
    "kind"        varchar(10)
-}
createDefault :: CreateTable
createDefault =
    createTable "films" cols
    where
        cols = [
              column "code" /++ char 5
            , column "title" /++ varchar 40 /++ defaultValue (toValue "Luso Films")
            , column "did" /++ integer
            , column "date_prod" /++ date
            , column "kind" /++ varchar 10
            ]

{-|        
CREATE TABLE "films" (
    "code"        char(5),
    "title"       varchar(40) NOT NULL,
    "did"         integer,
    "date_prod"   date,
    "kind"        varchar(10)
-}
createNotNull :: CreateTable
createNotNull =
    createTable "films" cols
    where
        cols = [
              column "code" /++ char 5
            , column "title" /++ varchar 40 /++ notNull
            , column "did" /++ integer
            , column "date_prod" /++ date
            , column "kind" /++ varchar 10
            ]

{-|        
CREATE TABLE "films" (
    "code"        char(5) UNIQUE,
    "title"       varchar(40),
    "did"         integer,
    "date_prod"   date,
    "kind"        varchar(10)
-}       
createUnique :: CreateTable
createUnique =
    createTable "films" cols
    where
        cols = [
              column "code" /++ char 5 /++ unique
            , column "title" /++ varchar 40
            , column "did" /++ integer
            , column "date_prod" /++ date
            , column "kind" /++ varchar 10
            ]
{-|
CREATE TABLE "distributors" (
    "did"     integer,
    "name"    varchar(40),
    UNIQUE("name")
)
-}       
createUniqueT :: CreateTable
createUniqueT =
    createTable "distributors" cols /++ [tableConstraint "" $ uniqueT [name]]
    where
        cols = [did, name]
        did = column "did" /++ integer
        name = column "name" /++ varchar 40

{-|        
CREATE TABLE weather (
    city      varchar(80) references cities(city),
    temp_lo   integer,
    temp_hi   integer,
    prcp      real,
    date      date
)
-}
createForeignKey :: CreateTable
createForeignKey =
    createTable "weather" cols
    where
        cols = [
              column "city" /++ varchar 80 /++ foreignKey "cities" "city"
            , column "temp_lo" /++ integer
            , column "temp_hi" /++ integer
            , column "prcp" /++ integer
            , column "date" /++ date
            ]