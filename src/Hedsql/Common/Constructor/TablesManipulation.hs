{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Hedsql/Common/Constructor/TablesManipulation.hs
Description : CREATE and DROP SQL statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for table manipulation statements.
Currently only CREATE and DROP statements are implemented.
ALTER statements are in project.
-}
module Hedsql.Common.Constructor.TablesManipulation
    (
      check
    , checkT
    , colConstraint
    , createTable
    , createTableIfNotExist
    , createView
    , defaultValue
    , dropTable
    , dropTableIfExists
    , dropView
    , foreignKey
    , notNull
    , null
    , nullVal
    , primary
    , primaryT
    , tableConstraint
    , unique
    , uniqueT
    ) where

import Hedsql.Common.Constructor.Columns
import Hedsql.Common.Constructor.Conditions
import Hedsql.Common.Constructor.Tables
import Hedsql.Common.DataStructure.Base

import Prelude hiding (null)

-- TODO: implement ALTER statements.

-- private functions.

-- | Return nothing if the provided string is empty.
maybeString :: String -> Maybe String
maybeString "" = Nothing
maybeString name = Just name

-- public functions.

-- | Create a CHECK constraint.
check :: ConditionConstruct a => a -> ColConstraintType
check condition = Check $ toCondition condition

-- | Create a CHECK constraint to be used in a table constraint.
checkT :: ConditionConstruct a => a -> TableConstraintType
checkT = TableConstraintCheck . toCondition

-- | Create a constraint which shall then be applied on a column.
colConstraint :: String -> ColConstraintType -> ColConstraint
colConstraint name constraintType =
    ColConstraint (maybeString name) constraintType

-- | Create a CREATE TABLE statement.
createTable :: TableConstructor a => a -> [Column] -> CreateTable
createTable t cols = CreateTable False (table t) cols Nothing

-- | Create a CREATE TABLE IF NOT EXIST statement.
createTableIfNotExist :: TableConstructor a => a -> [Column] -> CreateTable
createTableIfNotExist t cols = CreateTable True (table t) cols Nothing

-- | Create a CREATE VIEW query.
createView ::
       String      -- ^ Name of the view.
    -> SelectQuery -- ^ Select query from which the view is created.
    -> CreateView
createView name select = CreateView name select

-- | Create a DEFAULT value constraint.
defaultValue :: ColRefConstruct a => a -> ColConstraintType
defaultValue = Default . toExpr

-- | Create a DROP TABLE statement.
dropTable ::
       String -- ^ Name of the table. 
    -> DropTable
dropTable = DropTable False . table

dropTableIfExists ::
       TableConstructor a
    => a -- ^ Table or name of the table.
    -> DropTable
dropTableIfExists name = DropTable True $ table name

-- | Create a DROP VIEW query.
dropView ::
       String -- ^ Name of the view.
    -> DropView
dropView = DropView

-- | Create a FOREIGN KEY constraint.
foreignKey :: (TableConstructor a, ColumnConstructor b)
           => a -- ^ Table.
           -> b -- ^ Column.
           -> ColConstraintType
foreignKey t c = Reference (table t) (column c) Nothing

-- | Create a NOT NULL constraint.
notNull :: ColConstraintType
notNull = NotNull

-- | Create a NULL constraint.
null :: ColConstraintType
null = Null

-- | Create a NULL value.
nullVal :: SqlValue
nullVal = SqlValueNull

-- | Create a PRIMARY KEY constraint.
primary ::
       Bool -- ^ If True, the primary key will be an AUTOINCREMENT.
    -> ColConstraintType
primary = Primary

-- | Create a PRIMARY KEY constraint to be used in a table constraint.
primaryT :: ColumnConstructor a => a -> TableConstraintType
primaryT = TableConstraintPrimaryKey . columns

-- | Create a table constraint.
tableConstraint :: String -> TableConstraintType -> TableConstraint
tableConstraint name constraintType =
    TableConstraint (maybeString name) constraintType Nothing

-- | Create an UNIQUE column constraint.
unique :: ColConstraintType
unique = Unique

-- | Create an UNIQUE table constraint
uniqueT :: ColumnConstructor a => [a] -> TableConstraintType
uniqueT cols = TableConstraintUnique (map column cols)