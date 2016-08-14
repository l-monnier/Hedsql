{-|
Module      : Database/Hedsql/Statements/Insert.hs
Description : Collection of INSERT statements.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of INSERT statements to be used in tests or as examples.
-}
module Database.Hedsql.Statements.Insert
    (
      -- * All vendors
      juliusCeasar
    , gaiusJuliusCeasar
    , falseAge
    , withCols
    , defaultVal

      -- * PostgreSQL
    , defaultValPostgreSQL
    , returningPostgreSQL
    )
    where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.Ext
import           Database.Hedsql.SqLite
import qualified Database.Hedsql.PostgreSQL                      as P
import           Database.Hedsql.Drivers.PostgreSQL.Constructor

import           Prelude                                         hiding (null)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- Define the columns
idC :: Column Int dbVendor
idC = col "id" integer

title :: Column String dbVendor
title = col "title" $ char 2

firstName :: Column String dbVendor
firstName = col "firstName" $ varchar 256

lastName :: Column String dbVendor
lastName = col "lastName" $ varchar 256

age :: Column Int dbVendor
age = col "age" integer

married :: Column Bool dbVendor
married    = col "married" boolean

passportNo :: Column String dbVendor
passportNo = col "passportNo" $ varchar 256

father :: Column Int dbVendor
father = col "father" integer

countryId :: Column Int dbVendor
countryId = col "countryId" integer

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All vendors
----------------------------------------

{-|
MariaDB and PosgreSQL:
> INSERT INTO "People" ("id", "title", "firstName", "lastName", "age",
> "married", "passportNo", "countryId", "father")
> VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, TRUE, NULL, 1, 2)

SqLite:
> INSERT INTO "People" ("id", "title", "firstName", "lastName", "age",
> "married", "passportNo", "father", "countryId")
> VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, 1, NULL, 2, 2)
-}
juliusCeasar :: Insert Void dbVendor
juliusCeasar =
    insert "People"
        [ assign idC        $ intVal 1
        , assign title      $ stringVal "Mr"
        , assign firstName  $ stringVal "Julius"
        , assign lastName   $ stringVal "Ceasar"
        , assign age        $ intVal 2000
        , assign married    $ boolVal True
        , assign passportNo   null
        , assign father     $ intVal 2
        , assign countryId  $ intVal 2
        ]
    |> end

{-|
Use the generic 'value' constructor instead of the specialised ones.

MariaDB and PosgreSQL:
> INSERT INTO "People"
> VALUES (1, 'Mr', 'Gaius Julius', 'Ceasar', 2000, TRUE, NULL, NULL, 2)

SqLite:
> INSERT INTO "People"
> ("id", "title", "firstName", "lastName", "age", "married", "passportNo"
> , "father", "countryId")
> VALUES (1, 'Mr', 'Gaius Julius', 'Ceasar', 2000, 1, NULL, NULL, 2)
-}
gaiusJuliusCeasar :: Insert Void dbVendor
gaiusJuliusCeasar =
    insert "People"
        [ assign idC        $ value (2::Int)
        , assign title      $ value "Mr"
        , assign firstName  $ value "Gaius Julius"
        , assign lastName   $ value "Ceasar"
        , assign age        $ value (2000::Int)
        , assign married    $ value True
        , assign passportNo   null
        , assign father       null
        , assign countryId  $ value (2::Int)
        ]
    |> end

{-|
The below statement is going to fail, because the age is below 0.
> INSERT INTO "People"
> ("title", "firstName", "lastName", "age", "married", "passportNo", "father",
> "countryId")
> VALUES (NULL, 'Mr', 'Julius', 'Ceasar', -1, TRUE, NULL, NULL, 2)
-}
falseAge :: Insert Void dbVendor
falseAge =
    insert "People"
        [ assign title        null
        , assign firstName  $ stringVal "Julius"
        , assign lastName   $ stringVal "Ceasar"
        , assign age        $ intVal (-1)
        , assign married    $ boolVal True
        , assign passportNo   null
        , assign father       null
        , assign countryId  $ intVal 2
        ]
    |> end

{-|
@
INSERT INTO "People"
  ("title", "firstName", "lastName", "age", "married", "passportNo"
  , "countryId")
  VALUES ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2)
@
-}
withCols :: Insert Void dbVendor
withCols =
    insert
        "People"
        [ assign title       $ value "Mr"
        , assign firstName   $ value "Julius"
        , assign lastName    $ value "Ceasar"
        , assign age         $ intVal 2000
        , assign married       null
        , assign passportNo    null
        , assign countryId   $ intVal 2
        ]
    |> end

{-|
This example doesn't define the types of the columns.
@
INSERT INTO "People" (
  "firstName",
  "lastName",
  "age",
  "passportNo",
  "countryId")
VALUES (
  'Julius',
  'Ceasar',
  2000,
  NULL,
  2)
@
-}
defaultVal :: Insert Void dbVendor
defaultVal =
    insert
        "People"
        [ assign "firstName"  $ genQVal "Julius"
        , assign "lastName"   $ genQVal "Ceasar"
        , assign "age"        $ genVal (2000::Int)
        , assign "passportNo"   null
        , assign "countryId"  $ genVal (2::Int)
        ]
    |> end

----------------------------------------
-- PostgreSQL
----------------------------------------

{-|
> INSERT INTO "People"
> ("title", "firstName", "lastName", "age", "passportNo", "father", "countryId")
> VALUES (DEFAULT, 'Mr', 'Julius', 'Ceasar', 2000, TRUE, NULL, NULL, 2)
-}
defaultValPostgreSQL :: Insert Void P.PostgreSQL
defaultValPostgreSQL =
    insert "People"
        [ assign idC        $ null
        , assign title      $ value default_
        , assign firstName  $ stringVal "Julius"
        , assign lastName   $ stringVal "Ceasar"
        , assign age        $ intVal 2000
        , assign married    $ boolVal True
        , assign passportNo   null
        , assign father       null
        , assign countryId  $ intVal 2
        ]
    |> end

{-|
@
INSERT INTO "People" (
  "title",
  "firstName",
  "lastName",
  "age",
  "married",
  "passportNo",
  "countryId",
  "father")
VALUES (
  'Mr',
  'Julius',
  'Ceasar',
  2000,
  TRUE,
  NULL,
  1,
  2)
@
-}
returningPostgreSQL :: Insert Int P.PostgreSQL
returningPostgreSQL =
       insert "People"
           [ assign title      $ stringVal "Mr"
           , assign firstName  $ stringVal "Julius"
           , assign lastName   $ stringVal "Ceasar"
           , assign age        $ intVal 2000
           , assign married    $ boolVal True
           , assign passportNo   null
           , assign father     $ intVal 2
           , assign countryId  $ intVal 2
           ]
    |> P.returning idC
    |> end

{-|
@
INSERT INTO "People"
  ("title", "firstName", "lastName", "age", "passportNo", "countryId")
  VALUES
  ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2),
  ('Mr', 'Gnaeus', 'Pompeius', 2000, NULL, NULL, 2)
@
-}
--multiValsPostgreSQL :: Insert P.PostgreSQL
--multiValsPostgreSQL =
--    insert
--        "People"
--        ["title", "firstName", "lastName", "age", "passportNo", "countryId"]
--        [
--        [ wrap $ value "Mr"
--        , wrap $ value "Julius"
--        , wrap $ value "Ceasar"
--        , wrap $ value (2000::Int)
--        , wrap $ value null
--        , wrap $ value null
--        , wrap $ value (2::Int)
--        ]
--        ,
--        [ wrap $ value "Mr"
--        , wrap $ value "Gnaeus"
--        , wrap $ value "Pompeius"
--        , wrap $ value (2000::Int)
--        , wrap $ value null
--        , wrap $ value null
--        , wrap $ value (2::Int)
--        ]
--        ]
