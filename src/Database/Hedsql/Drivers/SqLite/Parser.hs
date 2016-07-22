{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

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
    ( T.Parser
    , parse
    , parseP
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.Common.AST
import           Database.Hedsql.Common.Constructor
import           Database.Hedsql.Common.Parser
import           Database.Hedsql.Drivers.SqLite.Driver
import qualified Database.Hedsql.Common.Parser.Type as T

import           Data.Text.Lazy                        (pack, toUpper)

import           Database.Hedsql.Common.PrettyPrint

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Parse SqLite data types.
sqLiteDataTypeFunc :: DataTypeWrap SqLite -> Doc
sqLiteDataTypeFunc dataType =
    case dataType of
        DataTypeWrap (Char lenght) ->
            "CHARACTER" <> parens (int lenght)
        _ ->
            text $ toUpper $ renderRaw $ parseDataTypeFunc dataType

{-|
Parse a SqLite value.
Booleans in SqLite are represented by numeric values only (0 or 1).
-}
sqLiteParseValueFunc :: ValueWrap SqLite -> Doc
sqLiteParseValueFunc (ValueWrap (BoolVal True))  = "1"
sqLiteParseValueFunc (ValueWrap (BoolVal False)) = "0"
sqLiteParseValueFunc val = parseValueFunc sqLiteParser val

-- | Create the SqLite function parser.
sqLiteExprFunc :: ExprWrap SqLite -> Doc
sqLiteExprFunc (ExprWrap CurrentDate) = "Date('now')"
sqLiteExprFunc (ExprWrap LastInsertId) = "last_insert_rowid()"
sqLiteExprFunc e = parseExprFunc sqLiteParser e

-- | Create the SqLite parser.
sqLiteParser :: Parser SqLite
sqLiteParser = Parser
    (parseStmtFunc sqLiteParser)
    sqLiteExprFunc
    (parseTableConstFunc sqLiteParser)
    (parseTableConstTypeFunc sqLiteParser)
    (parseFkFunc sqLiteParser)
    parseMatchFunc
    (parseOnActionFunc sqLiteParser)
    parseActionFunc
    parseConstTimingFunc
    (parseViewFunc sqLiteParser)
    (parseColCreateFunc sqLiteParser)
    sqLiteDataTypeFunc
    (parseColConstFunc sqLiteParser)
    (parseColConstTypeFunc sqLiteParser)
    (parseCreateFunc sqLiteParser)
    (parseDropFunc sqLiteParser)
    (parseTableNameFunc sqLiteParser)
    (parseTableRefFunc sqLiteParser)
    (parseTableRefAsFunc sqLiteParser)
    (parseColFunc sqLiteParser)
    (parseColDefFunc sqLiteParser)
    (parseColRefDefFunc sqLiteParser)
    (parseColRefFunc sqLiteParser)
    sqLiteParseValueFunc
    (parseSelectFunc sqLiteParser)
    parseCombinationFunc
    (parseSelectTypeFunc sqLiteParser)
    (parseSelectionFunc sqLiteParser)
    (parseFromFunc sqLiteParser)
    (parseJoinFunc sqLiteParser)
    (parseJoinClauseFunc sqLiteParser)
    parseJoinTColFunc
    parseJoinTTableFunc
    (parseWhereFunc sqLiteParser)
    (parseGroupByFunc sqLiteParser)
    (parseHavingFunc sqLiteParser)
    (parseOrderByFunc sqLiteParser)
    (parseSortRefFunc sqLiteParser)
    parseSortOrderFunc
    parseSortNullFunc
    (parseAssgnmtFunc sqLiteParser)
    (parseInsertAssignFunc sqLiteParser)
    (parseDeleteFunc sqLiteParser)
    (parseInsertFunc sqLiteParser)
    (parseUpdateFunc sqLiteParser)
    (parseReturningFunc sqLiteParser)
    quoteElemFunc
    quoteValFunc

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: T.Parser SqLite
parse = renderRaw ._parseStmt sqLiteParser . statement

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string in pretty print mode.
-}
parseP :: T.Parser SqLite
parseP = renderP ._parseStmt sqLiteParser . statement

