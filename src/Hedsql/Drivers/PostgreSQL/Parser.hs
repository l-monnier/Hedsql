-- file : Hedsql/Drivers/PostgreSQL/Parser.hs

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    SQLite parser implementation.
-}

module Hedsql.Drivers.PostgreSQL.Parser (
      module Hedsql.Common.DefaultParser
    , postgreSQLParser
    , toSqlString
    ) where

import Control.Lens
import Hedsql.Common.DataStructure.Base
import Hedsql.Common.Driver 
import Hedsql.Common.DefaultParser
import Hedsql.Common.Parser
import Hedsql.Common.Quoter
import Hedsql.Drivers.PostgreSQL.Driver

-- Private.

-- | PostgreSQL quoter.
postgreSqlQuoter :: Q PostgreSQL
postgreSqlQuoter = Q quoteElemFunc quoteValFunc

-- Public.

instance Parseable (DropTable PostgreSQL) where
    parse a = (postgreSQLParser^.parseDropTable) a postgreSQLParser

-- | PostgreSQL parser.
postgreSQLParser :: P PostgreSQL
postgreSQLParser = P
    parseTableNameFunc
    parseDropTableFunc
    parseDropViewFunc
    postgreSqlQuoter

{-|
    Parser instance for the PostgreSQL driver. It is basically a call to the
    "toSqlLiteString" function of the "PostgreSQLParser" type class.
-}
instance (DefaultParser PostgreSQL b, PostgreSQLParser b) => Parser PostgreSQL b where
    toSqlString _ b = toPostgreSQLString b
 
-- | PostgreSQL parser class.
class PostgreSQLParser a where
    toPostgreSQLString :: (DefaultParser PostgreSQL a) => a -> String

{-|
    Generic default instance used if no specific instance exists for the type.
    This is basically a call to the "toDefaultSqlString" function of the
    "DefaultParser" type class.
-}
instance PostgreSQLParser a where
    toPostgreSQLString = toDefaultSqlString PostgreSQL

{-|
    The AUTOINCREMENT constraint is not a constraint in PostgreSQL.
    Instead, the "serial" data type is used.
    
    We must therefore remove the AUTOINCREMENT constraint when building
    a PRIMARY column constraint.
-}
instance PostgreSQLParser ColConstraintType where
    toPostgreSQLString (Primary isAutoIncrement) = "PRIMARY KEY"
    toPostgreSQLString constraint = toDefaultSqlString PostgreSQL constraint
    
{- |
    Custom instance for PostgreSQL regarding the creation of a table.
    The difference with the default implementation is that a PRIMARY KEY of
    type Integer with an AUTOINCREMENT constraints get translated as a "Serial".
-}
instance PostgreSQLParser CreateTable where
    
    toPostgreSQLString stmt =
        makeCreateStatement PostgreSQL stmt makePostgreSQLColumn

-- | Build a column to be used in a CREATE statement for PostgreSQL.
makePostgreSQLColumn driver column =
       quoteSql PostgreSQL (column^.colName)
    ++ makePostgreSQLDataType (column^.colDataType) (column^.colConstraints)
    ++ makeColumnConstraints PostgreSQL (column^.colConstraints)

-- | Build a SQL data type to be used in CREATE statement for PostgreSQL.    
makePostgreSQLDataType (Just Integer) (Just constraints) =
    if hasAutoIncrementPrimaryKey constraints
    then " serial"
    else " integer"
makePostgreSQLDataType dataType contraints = makeDataType PostgreSQL dataType

{-|
    Return True if one of the provided constraint is a PRIMARY KEY.
    with auto increment.
-}
hasAutoIncrementPrimaryKey :: [ColConstraint] -> Bool
hasAutoIncrementPrimaryKey constraints =
    all check constraints
    where
        check constraint = isAutoIncrementPK $ constraint^.colConstraintType
        isAutoIncrementPK (Primary isAutoIncrement) = isAutoIncrement
        isAutoIncrementPK _                         = False       