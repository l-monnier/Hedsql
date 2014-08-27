-- file : Hedsql/Drivers/MariaDB/Driver.hs

{-|
    MariaDB driver. MariaDB being a fork of MySQL, this driver may also be used
    with a MySQL database.
-}

module Hedsql.Drivers.MariaDB.Driver where

import Hedsql.Commun.Driver

-- | MariaDB driver
data MariaDB = MariaDB
    
instance Driver MariaDB where
    getName driver = "MariaDB"