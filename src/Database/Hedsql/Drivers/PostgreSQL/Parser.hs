{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

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
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------
   
import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor.Statements 
import Database.Hedsql.Common.Parser
import Database.Hedsql.Drivers.PostgreSQL.Driver

import Control.Lens
import Data.List (intercalate)

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
parsePostgreSQLColConstTypeFunc :: Parser a -> ColConstraintType a -> String
parsePostgreSQLColConstTypeFunc parser constraint =
    case constraint of
        (Primary _) -> "PRIMARY KEY"
        _           -> parseColConstTypeFunc parser constraint

{- |
    Custom function for PostgreSQL for the creation of a table.
    The difference with the default implementation is that a PRIMARY KEY of
    type Integer with an AUTOINCREMENT constraints get translated as a "serial".
-}
parsePostgreSqlColCreateFunc :: Parser a -> ColWrap a -> String
parsePostgreSqlColCreateFunc parser (ColWrap col) =
        parseCols (DataTypeWrap $ col^.colType) (col^.colConstraints)        
    where
        parseCols (DataTypeWrap Integer) colConsts@(_:_) =
            if hasAutoIncrement colConsts
            then cName ++ " serial"  ++ consts colConsts
            else cName ++ " integer" ++ consts colConsts 
        parseCols cType colConsts = concat
            [ cName
            , " " ++ _parseDataType parser cType
            , consts colConsts
            ]
        
        cName = _quoteElem parser $ col^.colName
        
        consts [] = ""
        consts cs = " " ++ intercalate ", " (map (_parseColConst parser) cs) 

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: ToStmt (a PostgreSQL) (Statement PostgreSQL) => a PostgreSQL -> String
parse = _parseStmt postgreSQLParser . statement