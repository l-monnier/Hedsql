{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Database/Hedsql/Drivers/SqLite/Parser.hs
Description : SqLite parser.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SqLite parser implementation.
-}
module Database.Hedsql.Drivers.SqLite.Parser
    ( parse
    ) where
    
import Database.Hedsql.Common.Constructor.Statements
import Database.Hedsql.Common.DataStructure
import Database.Hedsql.Common.Parser
import Database.Hedsql.Drivers.SqLite.Driver

import qualified Database.Hedsql.Common.Parser.TableManipulations as T

import Control.Lens

import Data.Char

-- Private.

-- | Parse SqLite data types.
sqLiteDataTypeFunc :: DataTypeWrap SqLite -> String
sqLiteDataTypeFunc dataType =
    case dataType of
        DataTypeWrap (Char lenght) -> "CHARACTER(" ++ show lenght ++ ")"
        _                          -> map toUpper $ T.parseDataTypeFunc dataType

{-|
Parse a SqLite value.
Booleans in Sqlite are represented bynumeric values only (0 or 1).
-}
sqLiteParseValueFunc :: QueryParser SqLite -> ValueWrap SqLite -> String
sqLiteParseValueFunc _ (ValueWrap (BoolVal True))  = "1"
sqLiteParseValueFunc _ (ValueWrap (BoolVal False)) = "0"
sqLiteParseValueFunc parser val = parseValueFunc parser val

-- | Create the SqLite function parser.
sqLiteExprFunc :: ExprWrap SqLite -> String
sqLiteExprFunc (ExprWrap CurrentDate) = "Date('now')"
sqLiteExprFunc e = parseExprFunc sqLiteQueryParser sqLiteStmtParser e

-- | Create the SqLite table manipulations parser.
sqLiteTableParser :: T.TableParser SqLite
sqLiteTableParser =
    getTableParser sqLiteQueryParser sqLiteTableParser
        & T.parseDataType .~ sqLiteDataTypeFunc

-- | Create the SqLite parser.
sqLiteParser :: Parser SqLite
sqLiteParser = getParser sqLiteStmtParser
    
-- | Create the SqLite query parser.
sqLiteQueryParser :: QueryParser SqLite
sqLiteQueryParser =
    getQueryParser sqLiteQueryParser sqLiteTableParser
        & parseExpr .~ sqLiteExprFunc
        & parseValue .~ sqLiteParseValueFunc sqLiteQueryParser

sqLiteStmtParser :: StmtParser SqLite
sqLiteStmtParser = getStmtParser sqLiteQueryParser sqLiteTableParser

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: ToStmt (a SqLite) (Statement SqLite) => a SqLite -> String
parse = (sqLiteParser^.parseStmt).statement