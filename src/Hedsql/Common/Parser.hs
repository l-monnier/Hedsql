-- file : Hesql/Common/Parser
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
   Generic parser which converts data types to strings written in SQL (Search Query Language).
   Those strings shall be different depending on the driver used since each vendors (such as
   MariaDB, PostgreSQL, SQLite) has its own simplementation of SQL.
-}

module Hedsql.Common.Parser where
import Hedsql.Common.Driver

-- | Generic parser type class which defines the "toSqlString" function.
class Parser a b where
    -- | Convert a [SQL] data type to a [SQL] string.
    toSqlString :: Driver a =>
           a -- ^ A driver is a defined implementation of SQL by a vendor
             --   (such as Maria DB, PostgreSQL, SQLite).
        -> b -- ^ Data type to be converted to a string.
        -> String -- ^ The returned string.