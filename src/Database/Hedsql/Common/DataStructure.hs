{-|
Module      : Database/Hedsql/Common/DataStructure.hs
Description : Constructor functions for columns.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Data type system allowing the representation of SQL in haskell.
At the exception of its lenses, this module should not be directly used.
It is an "internal machinery" on which are then builds classes allowing the
easy generation of such SQL data which can then be later convert them to
SQL strings.
-}
module Database.Hedsql.Common.DataStructure
    ( module Database.Hedsql.Common.DataStructure.Create
    , module Database.Hedsql.Common.DataStructure.Delete
    , module Database.Hedsql.Common.DataStructure.Drop
    , module Database.Hedsql.Common.DataStructure.Insert
    , module Database.Hedsql.Common.DataStructure.Inspect
    , module Database.Hedsql.Common.DataStructure.Select
    , module Database.Hedsql.Common.DataStructure.Update
    , Statement
        ( CreateTableStmt
        , CreateViewStmt
        , DeleteStmt
        , DropTableStmt
        , DropViewStmt
        , InsertStmt
        , SelectStmt
        , UpdateStmt
        , Statements
        )
    ) where

import Database.Hedsql.Common.DataStructure.Create
import Database.Hedsql.Common.DataStructure.Delete
import Database.Hedsql.Common.DataStructure.Drop
import Database.Hedsql.Common.DataStructure.Insert
import Database.Hedsql.Common.DataStructure.Inspect
import Database.Hedsql.Common.DataStructure.Select
import Database.Hedsql.Common.DataStructure.Update

-- Public.

-- | All possible SQL statements which can be constructed using Hedsql.
data Statement a =
      CreateTableStmt (CreateTable a)
    | CreateViewStmt  (CreateView a)
    | DeleteStmt      (Delete a)
    | DropTableStmt   (DropTable a)
    | DropViewStmt    (DropView a)
    | InsertStmt      (Insert a)
    | SelectStmt      (Select a)
    | UpdateStmt      (Update a)
    | Statements      [Statement a] -- ^ Combination of many statements.
      deriving (Show)