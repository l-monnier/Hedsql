-- file : Hedsql/Drivers/PostgreSQL/Driver.hs

{-|
    PostgreSQL driver.
-}

module Hedsql.Drivers.PostgreSQL.Driver where

import Hedsql.Common.Driver

-- | SQLite driver
data PostgreSQL = PostgreSQL
    
instance Driver PostgreSQL where
    getName driver = "PostgreSQL"