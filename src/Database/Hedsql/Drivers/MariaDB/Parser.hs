{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Database/Hedsql/Drivers/MariaDB/Parser.hs
Description : MariaDB parser.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

MariaDB parser implementation.
-}
module Database.Hedsql.Drivers.MariaDB.Parser
    ( parse
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Parser
import Database.Hedsql.Drivers.MariaDB.Driver

import Data.Char

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Parse MariaDB data types.
mariaDBDataTypeFunc :: DataTypeWrap MariaDB -> String
mariaDBDataTypeFunc = map toUpper . parseDataTypeFunc

-- | Create the MariaDB parser.
mariaDBParser :: Parser MariaDB
mariaDBParser = getParser mariaDBParser {_parseDataType = mariaDBDataTypeFunc}

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: ToStmt (a MariaDB) (Statement MariaDB) => a MariaDB -> String
parse = _parseStmt mariaDBParser . statement