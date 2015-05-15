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

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.Parser
import Database.Hedsql.Drivers.SqLite.Driver

import Data.Char

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Parse SqLite data types.
sqLiteDataTypeFunc :: DataTypeWrap SqLite -> String
sqLiteDataTypeFunc dataType =
    case dataType of
        DataTypeWrap (Char lenght) -> "CHARACTER(" ++ show lenght ++ ")"
        _                          -> map toUpper $ parseDataTypeFunc dataType

{-|
Parse a SqLite value.
Booleans in SqLite are represented by numeric values only (0 or 1).
-}
sqLiteParseValueFunc :: ValueWrap SqLite -> String
sqLiteParseValueFunc (ValueWrap (BoolVal True))  = "1"
sqLiteParseValueFunc (ValueWrap (BoolVal False)) = "0"
sqLiteParseValueFunc val = parseValueFunc sqLiteParser val

-- | Create the SqLite function parser.
sqLiteExprFunc :: ExprWrap SqLite -> String
sqLiteExprFunc (ExprWrap CurrentDate) = "Date('now')"
sqLiteExprFunc e = parseExprFunc sqLiteParser e

-- | Create the SqLite parser.
sqLiteParser :: Parser SqLite
sqLiteParser =
    Parser
      (parseStmtFunc sqLiteParser)
      sqLiteExprFunc
      (parseTableFunc sqLiteParser)
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
      quoteElemFunc
      quoteValFunc

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: ToStmt a (Statement SqLite) => a -> String
parse = _parseStmt sqLiteParser . statement