-- file : Hedsql/Default.hs

{-|
    Default modules including default constructor, parser and driver
    to simplify the import.
-}
module Hedsql.Default
    (
      module Hedsql.Common.Driver
    , module Hedsql.Common.DefaultParser
    , module Hedsql.Common.Constructor
    , module Hedsql.Common.DataStructure.Base
    ) where

import Hedsql.Common.Driver
import Hedsql.Common.DefaultParser
import Hedsql.Common.Constructor
import Hedsql.Common.DataStructure