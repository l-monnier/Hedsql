-- file : Hedsql/Drivers/MariaDBParser.hs

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    MariaDB parser implementation.
-}

module Hedsql.Drivers.MariaDB.Parser where

import Hedsql.Commun.DataStructure
import Hedsql.Commun.Driver 
import Hedsql.Commun.DefaultParser
import Hedsql.Commun.Parser
import Hedsql.Drivers.MariaDB.Driver

data CalcFoundRows = CalcFoundRows deriving Show
data FoundRows= FoundRows deriving Show

instance DefaultParser MariaDB CalcFoundRows => Functionable CalcFoundRows where
    -- TODO: find a way to have it work for MariaDB only.
    toSqlFunctionString a b = toDefaultSqlString MariaDB b

instance DefaultParser MariaDB FoundRows => Functionable FoundRows where
    toSqlFunctionString a b = toDefaultSqlString MariaDB b

instance DefaultParser MariaDB CalcFoundRows where
    toDefaultSqlString MariaDB b = toMariaDBString b

{-|
    Parser instance for the MariaDB driver.
    It is basically a call to the "toMariaDBString" function of the
    "MariaDBParser" type class.
-}
instance (DefaultParser MariaDB b, MariaDBParser b) => Parser MariaDB b where
    toSqlString _ b = toMariaDBString b
 
-- | MariaDB parser class.
class MariaDBParser a where
    toMariaDBString :: (DefaultParser MariaDB a) => a -> String

{-|
    Generic default instance used if no specific instance exists for the type.
    This is basically a call to the "toDefaultSqlString" function of the
    "DefaultParser" type class.
-}
instance MariaDBParser a where
    toMariaDBString = toDefaultSqlString MariaDB

-- Instances specific to MariaDB

-- | SQL_CALC_FOUND_ROWS function.
instance MariaDBParser CalcFoundRows where
    toMariaDBString  _ = "SQL_CALC_FOUND_ROWS"
    
-- | FOUND_ROWS function.
instance MariaDBParser FoundRows where
    toMariaDBString  _ = "FOUND_ROWS()"