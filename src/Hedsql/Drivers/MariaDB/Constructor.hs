{-|
Module      : Hedsql/Drivers/MariaDB/Constructor.hs
Description : MariaDB specific constructors.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

MariaDB specific constructors for functions/clauses specific to this vendor.
-}
module Hedsql.Drivers.MariaDB.Constructor
    (
      calcFoundRows
    , foundRows
    )
    where

import Hedsql.Common.Constructor
import Hedsql.Common.DataStructure
import Hedsql.Drivers.MariaDB.Driver

-- Public.

-- | SQL_CALC_FOUND_ROWS function.
calcFoundRows :: Function MariaDB
calcFoundRows = CalcFoundRowsF CalcFoundRows

-- | FOUND_ROWS function.
foundRows :: Function MariaDB
foundRows = FoundRowsF FoundRows