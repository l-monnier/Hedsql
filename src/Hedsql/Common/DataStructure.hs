{-|
Module      : Hedsql/Common/DataStructure.hs
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
module Hedsql.Common.DataStructure
    ( module Hedsql.Common.DataStructure.Create
    , module Hedsql.Common.DataStructure.Delete
    , module Hedsql.Common.DataStructure.Drop
    , module Hedsql.Common.DataStructure.Insert
    , module Hedsql.Common.DataStructure.Inspect
    , module Hedsql.Common.DataStructure.Select
    , module Hedsql.Common.DataStructure.Update
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

import Hedsql.Common.DataStructure.Create
import Hedsql.Common.DataStructure.Delete
import Hedsql.Common.DataStructure.Drop
import Hedsql.Common.DataStructure.Insert
import Hedsql.Common.DataStructure.Inspect
import Hedsql.Common.DataStructure.Select
import Hedsql.Common.DataStructure.Update

-- Private.

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