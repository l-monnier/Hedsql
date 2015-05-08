{-# LANGUAGE EmptyDataDecls #-} 

{-|
Module      : Database/Hedsql/Drivers/MariaDB/Driver.hs
Description : MariaDB driver.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

MariaDB driver. MariaDB being a fork of MySQL, this driver may also be used
with a MySQL database.
-}
module Database.Hedsql.Drivers.MariaDB.Driver
    ( MariaDB
    ) where
    
-- Public.

-- | MariaDB driver.
data MariaDB