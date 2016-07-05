{-|
Module      : Database/Hedsql.hs
Description : Generic implementation.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Generic implementation including constructors only.

Using this module, you will be able to build generic queries which can then be
parsed by different parsers (SqLite, PostgreSQL and MariaDB).

You should use this module only if you wish to create generic implementations.
Otherwise, you will be better off using a specific module such as
'Database.Hedsql.SqLite'
-}
module Database.Hedsql (module H) where
import Database.Hedsql.Common.Constructor as H
import Database.Hedsql.Common.AST         as H
