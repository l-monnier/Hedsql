{-|
Module      : Database/Hedsql/Drivers/PostgreSQL.hs
Description : PostgreSQL implementation.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL implementation including constructor, parser and driver.

Using this module, you will be able to parse queries specifically for
PostgreSQL such as:

> select currentDate /++ from "table1"
> SELECT CURRENT_DATE FROM "table1"

If you want to load functionalities specific to PostgreSQL, load the following
module on top:
> import Hedsql.Drivers.PostgreSQL.Constructor

You will then be able to write queries such as:
> select "col1" /++ from (lateral (select "col1") "t1")
> SELECT "col1" FROM LATERAL (SELECT "col1") AS "t1"

Such queries cannot be parsed by another parser than the PostgreSQL one.
-}
module Database.Hedsql.PostgreSQL (module P) where

import Database.Hedsql.Drivers.PostgreSQL.Driver as P
import Database.Hedsql.Drivers.PostgreSQL.Parser as P
import Database.Hedsql.Common.Constructor as P
import Database.Hedsql.Common.DataStructure as P