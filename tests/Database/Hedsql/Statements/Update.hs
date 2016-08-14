{-|
Module      : Database/Hedsql/Statements/Update.hs
Description : Collection of UPDATE statements.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of UPDATE statements to be used in tests or as examples.
-}
module Database.Hedsql.Statements.Update
    (
      -- * All vendors
      equalTo
    , updateSelect

      -- * PostgreSQL
    , defaultVal
    , updateReturningClause
    )
    where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.Ext()
import           Database.Hedsql.SqLite
import           Database.Hedsql.Drivers.PostgreSQL.Constructor
import qualified Database.Hedsql.PostgreSQL                      as P

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All vendors
----------------------------------------

{-|
@
UPDATE "People"
SET "age" = 2050
WHERE "lastName" = 'Ceasar'
@
-}
equalTo :: Update Void dbVendor
equalTo =
       update "People" [assign (col "age" integer) $ intVal 2050]
    |> where_' (col "lastName" (varchar 256) /== value "Ceasar")
    |> end

{-|
UPDATE "People" SET "age" = "age" + 1 WHERE "countryId" IN
  (SELECT "countryId" FROM "Countries" WHERE "name" = 'Italy')
-}
updateSelect :: Update Void dbVendor
updateSelect =
       update "People" [assign age $ age /+ intVal 1]
    |> where_' (countryId `in_` execStmt subSelect)
    |> end
    where
        subSelect = do
            select countryId
            from "Countries"
            where_ (col "name" (varchar 256) /== value "Italy")
        countryId = col "countryId" (varchar 256)
        age = col "age" integer

----------------------------------------
-- PostgreSQL
----------------------------------------

-- | > UPDATE "People" SET "title" = DEFAULT WHERE "personId" = 1
defaultVal :: Update Void P.PostgreSQL
defaultVal =
       update "People" [assign "title" default_]
    |> where_' (col "personId" integer /== intVal 1)
    |> end

{-|
@
UPDATE "People"
SET "age" = 2050
WHERE "personId" = 1
RETURNING "id"
@
-}
updateReturningClause :: Update Int P.PostgreSQL
updateReturningClause =
       update "People" [assign (col "age" integer) $ intVal 2050]
    |> where_' (idC /== intVal 1)
    |> P.returning idC
    |> end
    where
       idC = col "personId" integer
