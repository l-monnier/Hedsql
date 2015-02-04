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
    ( Statement
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
     , module D
     ) where

import Database.Hedsql.Common.DataStructure.Delete  as D
import Database.Hedsql.Common.DataStructure.Drop    as D
import Database.Hedsql.Common.DataStructure.Insert  as D
import Database.Hedsql.Common.DataStructure.Inspect as D
import Database.Hedsql.Common.DataStructure.Select  as D
import Database.Hedsql.Common.DataStructure.Update  as D

-- Public.

-- | All possible SQL statements which can be constructed using Hedsql.
data Statement a =
      CreateTableStmt (Table a)
    | CreateViewStmt  (CreateView a)
    | DeleteStmt      (Delete a)
    | DropTableStmt   (DropTable a)
    | DropViewStmt    (DropView a)
    | InsertStmt      (Insert a)
    | SelectStmt      (Select a)
    | UpdateStmt      (Update a)
    | Statements      [Statement a] -- ^ Combination of many statements.
      deriving (Show)