{-|
Module      : Database/Hedsql/SqLite.hs
Description : SqLite implementation.
Copyright   : (c) Leonard Monnier, 2016
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
module Database.Hedsql.SqLite (module S) where
import Database.Hedsql.Drivers.SqLite.Driver as S
import Database.Hedsql.Drivers.SqLite.Parser as S
import Database.Hedsql.Common.Constructor    as S
import Database.Hedsql.Common.AST            as S
