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

import Database.Hedsql.Common.Constructor.Statements
import Database.Hedsql.Common.DataStructure
import Database.Hedsql.Common.Parser
import Database.Hedsql.Drivers.MariaDB.Driver
import qualified Database.Hedsql.Common.Parser.TableManipulations as T

import Control.Lens
import Data.Char

-- Private.

-- | Parse MariaDB data types.
mariaDBDataTypeFunc :: DataTypeWrap MariaDB -> String
mariaDBDataTypeFunc = map toUpper . T.parseDataTypeFunc

-- | Create the MariaDB parser.
mariaDBParser :: Parser MariaDB
mariaDBParser = getParser $ getStmtParser mariaDBQueryParser mariaDBTableParser
    
-- | Create the MariaDB query parser.
mariaDBQueryParser :: QueryParser MariaDB
mariaDBQueryParser = getQueryParser mariaDBQueryParser mariaDBTableParser

-- | Create the MariaDB table manipulations parser.
mariaDBTableParser :: T.TableParser MariaDB
mariaDBTableParser =
    getTableParser mariaDBQueryParser mariaDBTableParser
        & T.parseDataType    .~ mariaDBDataTypeFunc

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: ToStmt (a MariaDB) (Statement MariaDB) => a MariaDB -> String
parse = (mariaDBParser^.parseStmt).statement