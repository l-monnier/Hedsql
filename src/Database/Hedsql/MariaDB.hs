{-|
Module      : Database/Hedsql/Drivers/MariaDB.hs
Description : SqLite implementation.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

MariaDB implementation including constructor, parser and driver.

Using this module, you will be able to parse queries for MariaDB.

To write queries which are not parsable by the other drivers, you can
import the constructors dedicated to MariaDB:

> import Hedsql.Drivers.MariaDB.Constructor

You can then write queries as for example:

> select [colRef calcFoundRows, colRef (//*)] /++ from "table1"
> SELECT * CALC_FOUND_ROWS FROM `table1`
-}
module Database.Hedsql.MariaDB (module M) where

import Database.Hedsql.Drivers.MariaDB.Driver as M
import Database.Hedsql.Drivers.MariaDB.Parser as M
import Database.Hedsql.Common.Constructor as M
import Database.Hedsql.Common.DataStructure as M