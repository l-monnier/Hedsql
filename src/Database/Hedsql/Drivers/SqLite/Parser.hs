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

-- Private.

-- | Parse SqLite data types.
sqLiteDataTypeFunc :: SqlDataType SqLite -> String
sqLiteDataTypeFunc  Date         = "DATE"
sqLiteDataTypeFunc (Char lenght) = "CHARACTER(" ++ show lenght ++ ")"
sqLiteDataTypeFunc  SmallInt     = "SMALLINT"
sqLiteDataTypeFunc  Integer      = "INTEGER"
sqLiteDataTypeFunc  BigInt       = "BIGINT"
sqLiteDataTypeFunc (Varchar mx)  = "VARCHAR(" ++ show mx ++ ")"

-- | Create the SqLite function parser.
sqLiteFuncFunc :: Function SqLite -> String
sqLiteFuncFunc CurrentDate = "Date('now')"
sqLiteFuncFunc func        = parseFuncFunc sqLiteQueryParser func

-- | Create the SqLite table manipulations parser.
sqLiteTableParser :: T.TableParser SqLite
sqLiteTableParser =
    getTableParser sqLiteQueryParser sqLiteTableParser
        & T.parseDataType .~ sqLiteDataTypeFunc

-- | Create the SqLite parser.
sqLiteParser :: Parser SqLite
sqLiteParser = getParser $ getStmtParser sqLiteQueryParser sqLiteTableParser
    
-- | Create the SqLite query parser.
sqLiteQueryParser :: QueryParser SqLite
sqLiteQueryParser =
    getQueryParser sqLiteQueryParser sqLiteTableParser
        & parseFunc .~ sqLiteFuncFunc

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: ToStmt a Statement => a SqLite -> String
parse = (sqLiteParser^.parseStmt).statement