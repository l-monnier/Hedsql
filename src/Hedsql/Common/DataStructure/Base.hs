-- file : Hedsql/Common/DataStructure/base

{-|
    Data type system allowing the representation of SQL in haskell.
    At the exception of its lenses, this module should not be directly used.
    It is an "internal machinery" on which are then builds classes allowing the
    easy generation of such SQL data which can then be later convert them to
    SQL strings.
-}

module Hedsql.Common.DataStructure.Base (
      module Hedsql.Common.DataStructure.Create
    , module Hedsql.Common.DataStructure.Delete
    , module Hedsql.Common.DataStructure.Drop
    , module Hedsql.Common.DataStructure.Insert
    , module Hedsql.Common.DataStructure.Select
    , module Hedsql.Common.DataStructure.Update
    ) where

import Hedsql.Common.DataStructure.Create
import Hedsql.Common.DataStructure.Delete
import Hedsql.Common.DataStructure.Drop
import Hedsql.Common.DataStructure.Insert
import Hedsql.Common.DataStructure.Select
import Hedsql.Common.DataStructure.Update