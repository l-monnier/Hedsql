{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Hedsql/Drivers/MariaDB/Parser.hs
Description : MariaDB parser.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

MariaDB parser implementation.
-}
module Hedsql.Drivers.MariaDB.Parser
    ( parse
    ) where

import Hedsql.Common.Constructor.Statements
import Hedsql.Common.DataStructure
import Hedsql.Common.Parser
import Hedsql.Drivers.MariaDB.Driver

import Control.Lens

-- Private.

mariaDBFuncFunc :: Function MariaDB -> String
mariaDBFuncFunc CalcFoundRows = "SQL_CALC_FOUND_ROWS"
mariaDBFuncFunc FoundRows     = "FOUND_ROWS()"
mariaDBFuncFunc func          = parseFuncFunc mariaDBQueryParser func

-- | Create the MariaDB parser.
mariaDBParser :: Parser MariaDB
mariaDBParser = getParser $ getStmtParser mariaDBQueryParser genTableParser
    
-- | Create the MariaDB query parser.
mariaDBQueryParser :: QueryParser MariaDB
mariaDBQueryParser =
    (getQueryParser mariaDBQueryParser genTableParser)
        & parseFunc .~ mariaDBFuncFunc

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: CoerceToStmt a Statement => (a MariaDB) -> String
parse = (mariaDBParser^.parseStmt).statement