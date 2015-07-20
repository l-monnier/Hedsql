{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database/Hedsql/Drivers/PostgreSQL/Parser.hs
Description : PostgreSQL parser implementation.
Copyright   : (c) Leonard Monnier2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL parser implementation.
-}
module Database.Hedsql.Drivers.PostgreSQL.Parser
    ( parse
    , parseP
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.Parser
import Database.Hedsql.Drivers.PostgreSQL.Driver

import Control.Lens
import Database.Hedsql.Common.PrettyPrint

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

{-|
Return True if one of the provided constraint is a PRIMARY KEY.
with auto increment.
-}
hasAutoIncrement :: [ColConstraint a] -> Bool
hasAutoIncrement =
    all (\x -> isAIPK $ x^.colConstraintType)
    where
        isAIPK (Primary isAI) = isAI
        isAIPK _              = False

-- | Create the PostgreSQL parser.
postgreSQLParser :: Parser PostgreSQL
postgreSQLParser =
    getParser postgreSQLParser
        { _parseColConstType = parsePostgreSQLColConstTypeFunc postgreSQLParser
        , _parseColCreate    = parsePostgreSqlColCreateFunc    postgreSQLParser
        }

{-|
The AUTOINCREMENT constraint is not a constraint in PostgreSQL.
Instead, the "serial" data type is used.

We must therefore remove the AUTOINCREMENT constraint when parsing
a PRIMARY KEY column constraint.
-}
parsePostgreSQLColConstTypeFunc :: Parser a -> ColConstraintType a -> Doc
parsePostgreSQLColConstTypeFunc parser c =
    case c of
        (Primary _) -> "PRIMARY KEY"
        _           -> parseColConstTypeFunc parser c

{- |
    Custom function for PostgreSQL for the creation of a table.
    The difference with the default implementation is that a PRIMARY KEY of
    type Integer with an AUTOINCREMENT constraints get translated as a "serial".
-}
parsePostgreSqlColCreateFunc :: Parser a -> Int -> ColWrap a -> Doc
parsePostgreSqlColCreateFunc parser _ (ColWrap c) =
        parseCols (DataTypeWrap $ c^.colType) (c^.colConstraints)
    where
        parseCols (DataTypeWrap Integer) colConsts@(_:_) =
            if hasAutoIncrement colConsts
            then cName <+> "serial" <+> consts colConsts
            else cName <+> "integer" <+> consts colConsts
        parseCols cType colConsts = hsep
            [ cName
            , _parseDataType parser cType
            , consts colConsts
            ]

        cName = _quoteElem parser $ c^.colName

        consts [] = empty
        consts cs = csep $ map (_parseColConst parser) cs

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: ToStmt a (Statement PostgreSQL) => a -> String
parse = renderRaw . _parseStmt postgreSQLParser . statement

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string in pretty print mode.
-}
parseP :: ToStmt a (Statement PostgreSQL) => a -> String
parseP = show . _parseStmt postgreSQLParser . statement
