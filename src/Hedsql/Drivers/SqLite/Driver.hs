{-|
Module      : Hedsql/Drivers/SqLite/Driver.hs
Description : SqLite driver.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SqLite driver.
-}
module Hedsql.Drivers.SqLite.Driver
    ( SqLite
    ) where

-- | SQLite driver
data SqLite = SqLite