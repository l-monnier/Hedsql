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
    , getTableRefName
    ) where

import Database.Hedsql.Common.DataStructure.Select

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Control.Applicative
import Control.Lens
import Data.Maybe

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Return the alias of a table reference if there is one.
Otherwise return Nothing.
-}
getTableRefAlias :: TableRef a -> Maybe (TableRefAs a)
getTableRefAlias (LateralTableRef _ ref) = Just ref
getTableRefAlias (SelectTableRef  _ ref) = Just ref
getTableRefAlias (TableJoinRef    _ ref) =      ref
getTableRefAlias (TableTableRef   _ ref) =      ref

{-|
Return the name of a table reference.
If an alias exists: use the name of this alias.

Otherwise, if this is a table, use that table's name. Else, this is a join
without alias clause: return the names of the two table references
separated by "_". For examples: "Table1_Table2".
-}
getTableRefName :: TableRef a -> String
getTableRefName ref =
    fromMaybe getName $ (^. tableRefAliasName) <$> getTableRefAlias ref
    where
        getName =
            case ref of
                TableJoinRef (JoinTable _ table1 table2)    _ ->
                       getTableRefName table1
                    ++ "_"
                    ++ getTableRefName table2
                TableJoinRef (JoinColumn _ table1 table2 _) _ ->
                       getTableRefName table1
                    ++ "_"
                    ++ getTableRefName table2
                TableTableRef t                             _ ->
                    getTableName t
                _                                             ->
                    error "This is a bug, this pattern shouldn't been reached!"

getTableName :: Table a -> String
getTableName (Table _ name _ _) = name           