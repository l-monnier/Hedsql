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
    (
      parse
    ) where

import Hedsql.Common.Constructor.Statements
import Hedsql.Common.DataStructure
import Hedsql.Common.Parser
import Hedsql.Drivers.PostgreSQL.Driver

import Control.Lens

-- Private.

-- | Create the PostgreSQL parser.
postgreSQLParser :: Parser PostgreSQL
postgreSQLParser = getParser $ getStmtParser postgreSQLQueryParser

-- | Create the PostgreSQL query parser.
postgreSQLQueryParser :: QueryParser PostgreSQL
postgreSQLQueryParser = 
    getQueryParser postgreSQLQueryParser (getGenFuncParser postgreSQLQueryParser)

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: CoerceToStmt a Statement => (a PostgreSQL) -> String
parse = (postgreSQLParser^.parseStmt).statement

-- TODO: to be reworked.

--{-|
--    The AUTOINCREMENT constraint is not a constraint in PostgreSQL.
--    Instead, the "serial" data type is used.
--    
--    We must therefore remove the AUTOINCREMENT constraint when building
--    a PRIMARY column constraint.
---}
--instance PostgreSQLParser ColConstraintType where
--    toPostgreSQLString (Primary isAutoIncrement) = "PRIMARY KEY"
--    toPostgreSQLString constraint = toDefaultSqlString PostgreSQL constraint
--    
--{- |
--    Custom instance for PostgreSQL regarding the creation of a table.
--    The difference with the default implementation is that a PRIMARY KEY of
--    type Integer with an AUTOINCREMENT constraints get translated as a "Serial".
---}
--instance PostgreSQLParser CreateTable where
--    
--    toPostgreSQLString stmt =
--        makeCreateStatement PostgreSQL stmt makePostgreSQLColumn
--
-- | Build a column to be used in a CREATE statement for PostgreSQL.
--makePostgreSQLColumn driver column =
--       quoteSql PostgreSQL (column^.colName)
--    ++ makePostgreSQLDataType (column^.colDataType) (column^.colConstraints)
--    ++ makeColumnConstraints PostgreSQL (column^.colConstraints)
--
-- | Build a SQL data type to be used in CREATE statement for PostgreSQL.    
--makePostgreSQLDataType (Just Integer) (Just constraints) =
--    if hasAutoIncrementPrimaryKey constraints
--    then " serial"
--    else " integer"
--makePostgreSQLDataType dataType contraints = makeDataType PostgreSQL dataType
--
--{-|
--    Return True if one of the provided constraint is a PRIMARY KEY.
--    with auto increment.
---}
--hasAutoIncrementPrimaryKey :: [ColConstraint] -> Bool
--hasAutoIncrementPrimaryKey constraints =
--    all check constraints
--    where
--        check constraint = isAutoIncrementPK $ constraint^.colConstraintType
--        isAutoIncrementPK (Primary isAutoIncrement) = isAutoIncrement
--        isAutoIncrementPK _                         = False       