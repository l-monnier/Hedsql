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
import Hedsql.Common.Parser.Functions
import Hedsql.Drivers.MariaDB.Driver

import Control.Lens

-- Private.

-- | SQL_CALC_FOUND_ROWS function.
mariaDBCalcFoundRowsFunc :: CalcFoundRows MariaDB -> String
mariaDBCalcFoundRowsFunc _ = "SQL_CALC_FOUND_ROWS"
    
-- | FOUND_ROWS function.
mariaDBFoundRowsFunc :: FoundRows MariaDB -> String
mariaDBFoundRowsFunc _ = "FOUND_ROWS()"

-- | Create the MariaDB function parser.
mariaDBFuncParser :: FuncParser MariaDB
mariaDBFuncParser =
    (getGenFuncParser mariaDBQueryParser)
        & parseCalcFoundRows .~ mariaDBCalcFoundRowsFunc
        & parseFoundRows     .~ mariaDBFoundRowsFunc

-- | Create the MariaDB parser.
mariaDBParser :: Parser MariaDB
mariaDBParser = getParser $ getStmtParser mariaDBQueryParser
    
-- | Create the MariaDB query parser.
mariaDBQueryParser :: QueryParser MariaDB
mariaDBQueryParser = getQueryParser mariaDBQueryParser mariaDBFuncParser

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: CoerceToStmt a Statement => (a MariaDB) -> String
parse = (mariaDBParser^.parseStmt).statement