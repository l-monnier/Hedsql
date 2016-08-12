{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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
    ( T.Parser
    , parse
    , parseP
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Parser
import Database.Hedsql.Drivers.MariaDB.Driver
import qualified Database.Hedsql.Common.Parser.Type as T

import Data.Char
import Data.Text.Lazy(pack)
import Database.Hedsql.Common.PrettyPrint

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Parse MariaDB data types.
mariaDBDataTypeFunc :: DataTypeWrap MariaDB -> Doc
mariaDBDataTypeFunc = text . pack . map toUpper . show . parseDataTypeFunc

mariaDBExprFunc :: ExprWrap MariaDB -> Doc
mariaDBExprFunc (ExprWrap LastInsertId) = "LAST_INSERT_ID()"
mariaDBExprFunc e = parseExprFunc mariaDBParser e

-- | Create the MariaDB parser.
mariaDBParser :: Parser MariaDB
mariaDBParser = getParser mariaDBParser {
      _parseDataType = mariaDBDataTypeFunc
    , _parseExpr     = mariaDBExprFunc
    }

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: T.Parser MariaDB
parse = renderRaw . _parseStmt mariaDBParser . statement

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string in pretty print mode.
-}
parseP :: T.Parser MariaDB
parseP = show . _parseStmt mariaDBParser . statement
