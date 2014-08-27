-- file : Hedsql/PostgreSQL.hs

{-|
    PostgreSQL module including constructor, parser and driver
    to simplify the import.
-}

module Hedsql.PostgreSQL (
      module Hedsql.Drivers.PostgreSQL.Driver
    , module Hedsql.Drivers.PostgreSQL.Parser
    , module Hedsql.Common.Constructor
    , module Hedsql.Common.DataStructure.Base
    ) where

import Hedsql.Drivers.PostgreSQL.Driver
import Hedsql.Drivers.PostgreSQL.Parser
import Hedsql.Common.Constructor
import Hedsql.Common.DataStructure.Base