{-|
Module      : Hedsql/Common/Parser/Queries/Select.hs
Description : Collection of SELECT queries.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of SELECT queries to be used in tests or as examples.
-}
module Hedsql.Common.Parser.Queries.Select where

import Hedsql.SqLite

import qualified Hedsql.PostgreSQL                     as Pg
import qualified Hedsql.Drivers.PostgreSQL.Constructor as P

-- Selects.

-- | > SELECT * FROM "People"
selectAll :: Select a
selectAll = select (//*) /++ from "People"

-- | SELECT DISTINCT "firstName" FROM "People"
distinctSelect :: Select a
distinctSelect = selectDistinct "firstName" /++ from "People"

-- Functions.

-- | > SELECT ("age" + 1) FROM "People"
addition :: Select a
addition = select (colRef $ "age" /+ (1::Int)) /++ from "People"
    
-- | > SELECT (3 * 4)
multiplication :: Select a
multiplication = select $ (3::Int) /* (4::Int)

{-|
MariaDB & PostgreSQL
> SELECT CURRENT_DATE

SqLite
> SELECT Date('now')
-}
selectCurrentDate :: Select a
selectCurrentDate = select currentDate

{-|
MariaDB
> SELECT RAND()

PostgreSQL & SqLite
> SELECT random()
-}
selectRandom :: Select a
selectRandom = select random

-- From.

-- | SELECT * FROM "People" CROSS JOIN "Countries"
fromCrossJoin :: Select a
fromCrossJoin =  select (//*) /++  from ("People" `crossJoin` "Countries")

{-|
SELECT *
FROM "People"
    INNER JOIN "Countries" ON "(People"."countryId" = "Countries"."countryId)"
-}
fromInnerJoinOn :: Select a
fromInnerJoinOn =
        select (//*)
    /++ from (innerJoin "People" "Countries"
            $ "People" /. "countryId" /== "Countries" /. "countryId")

-- | SELECT * FROM "People" INNER JOIN "Countries" USING ("country")
fromInnerJoinUsing :: Select a
fromInnerJoinUsing =
    select (//*) /++ from (innerJoin "People" "Countries" "countryId")

-- Specifics to PostgreSQL

-- | SELECT DISTINCT ON ("firstName") * FROM "People" ORDER BY "age"
distinctOnSelect :: Select Pg.PostgreSQL
distinctOnSelect =
    P.selectDistinctOn "firstName" (//*) /++ from "People" /++ orderBy "age"