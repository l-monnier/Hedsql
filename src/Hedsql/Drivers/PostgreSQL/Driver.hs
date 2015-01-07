{-|
Module      : Hedsql/Drivers/PostgreSQL/Driver.hs
Description : PostgreSQL driver.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL driver.
-}
module Hedsql.Drivers.PostgreSQL.Driver
    (
      PostgreSQL
    ) where

-- | PostgreSQL driver.
data PostgreSQL = PostgreSQL