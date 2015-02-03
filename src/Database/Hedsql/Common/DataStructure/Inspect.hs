{-|
Module      : Database/Hedsql/Common/DataStructure/Inspect.hs
Description : Helper function to access the value of the SQL data structures.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Helper function to access the value of the SQL data structures.

All the SQL data structures of Hedsql have lenses, but sometimes they are not
the most convenient way to get some data. In this module you will find
functions which makes it easier.
-}
module Database.Hedsql.Common.DataStructure.Inspect
    ( getTableRefAlias
    ) where

import Database.Hedsql.Common.DataStructure.Select

-- Private functions.

-- Public functions.

{- |
Return the alias of a table reference if there is one.
Otherwise return Nothing.
-}
getTableRefAlias :: TableRef a -> Maybe (TableRefAs a)
getTableRefAlias (LateralTableRef _ ref) = Just ref
getTableRefAlias (SelectTableRef  _ ref) = Just ref
getTableRefAlias (TableJoinRef    _ ref) =      ref
getTableRefAlias (TableTableRef   _ ref) =      ref