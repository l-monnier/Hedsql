{-|
Module      : Hedsql/Drivers/SqLite.hs
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

module Hedsql.SqLite (
      module Hedsql.Drivers.SqLite.Driver
    , module Hedsql.Drivers.SqLite.Parser
    , module Hedsql.Common.Constructor
    , module Hedsql.Common.DataStructure
    ) where

import Hedsql.Drivers.SqLite.Driver
import Hedsql.Drivers.SqLite.Parser
import Hedsql.Common.Constructor
import Hedsql.Common.DataStructure