{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Hedsql/Drivers/PostgreSQL/Parser.hs
Description : PostgreSQL parser implementation.
Copyright   : (c) Leonard Monnier2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL parser implementation.
-}
module Hedsql.Drivers.PostgreSQL.Parser
    ( parse
    ) where

import Hedsql.Common.Constructor.Statements
import Hedsql.Common.DataStructure
import Hedsql.Common.Parser
import Hedsql.Drivers.PostgreSQL.Driver

import qualified Hedsql.Common.Parser.TableManipulations as T

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (catMaybes)

-- Private.

{-|
Return True if one of the provided constraint is a PRIMARY KEY.
with auto increment.
-}
hasAutoIncrement :: [ColConstraint a] -> Bool
hasAutoIncrement constraints =
    all (\x -> isAIPK $ x^.colConstraintType) constraints
    where
        isAIPK (Primary isAI) = isAI
        isAIPK _              = False  

-- | Create the PostgreSQL parser.
postgreSQLParser :: Parser PostgreSQL
postgreSQLParser =
    getParser $ getStmtParser postgreSQLQueryParser postgreSQLTableParser

-- | Create the PostgreSQL query parser.
postgreSQLQueryParser :: QueryParser PostgreSQL
postgreSQLQueryParser = 
    getQueryParser
        postgreSQLQueryParser
        postgreSQLTableParser

-- | Create the PostgreSQL table manipulations parser.
postgreSQLTableParser :: T.TableParser PostgreSQL
postgreSQLTableParser =
    (getTableParser postgreSQLQueryParser postgreSQLTableParser)
        & T.parseColConstType .~ colConstFunc
        & T.parseColCreate    .~ colCreateFunc
    where
        colConstFunc  = parsePostgreSQLColConstTypeFunc postgreSQLTableParser
        colCreateFunc = parsePostgreSqlColCreateFunc    postgreSQLTableParser

{-|
The AUTOINCREMENT constraint is not a constraint in PostgreSQL.
Instead, the "serial" data type is used.

We must therefore remove the AUTOINCREMENT constraint when parsing
a PRIMARY KEY column constraint.
-}
parsePostgreSQLColConstTypeFunc ::
    T.TableParser a -> ColConstraintType a -> String
parsePostgreSQLColConstTypeFunc parser constraint =
    case constraint of
        (Primary _) -> "PRIMARY KEY"
        _           -> T.parseColConstTypeFunc parser constraint

{- |
    Custom function for PostgreSQL for the creation of a table.
    The difference with the default implementation is that a PRIMARY KEY of
    type Integer with an AUTOINCREMENT constraints get translated as a "Serial".
-}
parsePostgreSqlColCreateFunc :: T.TableParser a -> Column a -> String
parsePostgreSqlColCreateFunc parser col =
        parseCols (col^.colDataType) (col^.colConstraints)        
    where
        parseCols (Just Integer) (Just colConsts) =
            if hasAutoIncrement colConsts
            then cName ++ " serial"  ++ consts colConsts
            else cName ++ " integer" ++ consts colConsts 
        parseCols colType colConsts = concat $ catMaybes
            [
              Just cName
            , fmap (\x -> " " ++ (parser^.T.parseDataType) x) colType
            , fmap consts                                     colConsts
            ]
        cName = parser^.T.quoteElem $ col^.colName
        consts cs =
            " " ++ (intercalate ", " $ map (parser^.T.parseColConst) cs) 

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: CoerceToStmt a Statement => (a PostgreSQL) -> String
parse = (postgreSQLParser^.parseStmt).statement  