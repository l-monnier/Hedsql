{-|
Module      : Hedsql/Drivers/MariaDB.hs
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
module Hedsql.MariaDB
    ( module Hedsql.Drivers.MariaDB.Driver
    , module Hedsql.Drivers.MariaDB.Parser
    , module Hedsql.Common.Constructor
    , module Hedsql.Common.DataStructure
    ) where

import Hedsql.Drivers.MariaDB.Driver
import Hedsql.Drivers.MariaDB.Parser
import Hedsql.Common.Constructor
import Hedsql.Common.DataStructure