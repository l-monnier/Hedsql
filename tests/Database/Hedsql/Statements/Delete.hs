{-|
Module      : Database/Hedsql/Statements/Delete.hs
Description : Collection of DELETE statements.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of DELETE statements to be used in tests or as examples.
-}
module Database.Hedsql.Statements.Delete
    (
      -- * All vendors
      deleteNotEqualTo
    , deleteSubQuery

      -- * PostgreSQL
    , deleteReturningClause

      -- * MariaDB
    , deleteReturningClauseMariaDB
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.SqLite
import qualified Database.Hedsql.MariaDB as M
import qualified Database.Hedsql.PostgreSQL as P

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

people :: Table a
people = table "People"

countries :: Table a
countries = table "Countries"

{-|
@
DELETE FROM "People"
WHERE "age" <> 20
@
-}
deleteNotEqualTo :: DeleteStmt' Void dbVendor
deleteNotEqualTo =
       deleteFrom people
    |> where_' (col "age" integer /<> value (20::Int))
    |> end

{-|
@
DELETE FROM "People"
WHERE "personId" IN (SELECT "personId"
                     FROM "Countries"
                     WHERE "name" = 'Switzerland')
@
-}
deleteSubQuery :: DeleteStmt' Void dbVendor
deleteSubQuery =
       deleteFrom people
    |> where_' (personId `in_`
           (execStmt $ do
               select personId
               from countries
               where_ (col "name" (varchar 128) /== value "Switzerland")
           )
       )
    |> end
    where
        personId = col "personId" integer

----------------------------------------
-- PostgreSQL
----------------------------------------

{-
@
DELETE FROM "People"
WHERE "age" = 20
RETURNING "personId"
@
-}
deleteReturningClause :: DeleteStmt' Int P.PostgreSQL
deleteReturningClause =
       deleteFrom people
    |> where_' (col "age" integer /== value (20::Int))
    |> P.returning (col "personId" integer)
    |> end

----------------------------------------
-- MariaDB
----------------------------------------

{-|
@
DELETE FROM "People"
WHERE "age" = 20
RETURNING "personId"
@
-}
deleteReturningClauseMariaDB :: DeleteStmt' Int M.MariaDB
deleteReturningClauseMariaDB =
       deleteFrom people
    |> where_' (col "age" integer /== value (20::Int))
    |> M.returning (col "personId" integer)
    |> end
