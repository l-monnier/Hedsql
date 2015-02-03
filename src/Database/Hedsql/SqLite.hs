{-|
Module      : Database/Hedsql/Drivers/SqLite.hs
Description : SqLite implementation.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SqLite implementation including constructor, parser and driver.

Using this module, you will be able to
build and parse queries which are specific to SqLite such as:

> select currentDate /++ from "table1"
> SELECT Date('now') FROM "table1"
-}
module Database.Hedsql.SqLite
    ( module Database.Hedsql.Drivers.SqLite.Driver
    , module Database.Hedsql.Drivers.SqLite.Parser
    , module Database.Hedsql.Common.Constructor
    , module Database.Hedsql.Common.DataStructure
    ) where

import Database.Hedsql.Drivers.SqLite.Driver
import Database.Hedsql.Drivers.SqLite.Parser
import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.DataStructure