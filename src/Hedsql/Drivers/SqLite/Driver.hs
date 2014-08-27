-- file : Hedsql/Drivers/SqLite/Driver.hs

{-|
    SQLite driver.
-}

module Hedsql.Drivers.SqLite.Driver where

import Hedsql.Common.Driver

-- | SQLite driver
data SqLite = SqLite
    
instance Driver SqLite where
    getName driver = "SQLite"