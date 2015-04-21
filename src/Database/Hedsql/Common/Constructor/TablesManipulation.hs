{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Database/Hedsql/Common/Constructor/TablesManipulation.hs
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
module Database.Hedsql.Common.Constructor.TablesManipulation
    ( 
      -- * Tables
    
      -- ** Create
      check
    , checkT
    , colConstraint
    , createTable
    , createTableIfNotExist
    , defaultValue
    , foreignKey
    , notNull
    , nullable
    , primary
    , primaryT
    , tableConstraint
    , unique
    , uniqueT
    
    -- ** Drop
    , dropTable
    , dropTableIfExists
    
      -- * Views
    , createView
    , createViewIfNotExist
    , dropView
    , dropViewIfExists
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST    
import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.Constructor.Tables

import Control.Lens
import Prelude      hiding (null)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- TODO: implement ALTER statements.

-- | Return nothing if the provided string is empty.
maybeString :: String -> Maybe String
maybeString ""   = Nothing
maybeString name = Just name

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Create a CHECK constraint.
check :: Expression Bool b -> ColConstraintType b
check = Check

-- | Create a CHECK constraint to be used in a table constraint.
checkT :: Expression Bool b -> TableConstraintType b
checkT = TCCheck

-- | Create a constraint which shall then be applied on a column.
colConstraint :: String -> ColConstraintType a -> ColConstraint a
colConstraint name = ColConstraint (maybeString name)

-- | Create a CREATE TABLE statement.
createTable :: ToTables a [Table b] => a -> [ColWrap b] -> Create b
createTable t c = CreateTable False $ table t & tableCols .~ c

-- | Create a CREATE TABLE IF NOT EXIST statement.
createTableIfNotExist :: ToTables a [Table b] => a -> [ColWrap b] -> Create b
createTableIfNotExist t c = CreateTable True (table t & tableCols .~ c)

-- | Create a CREATE VIEW statement.
createView ::
       View a   -- ^ Select query from which the view is created.
    -> Create a
createView = CreateView False

-- | Create a CREATE VIEW IF NOT EXIST statement.
createViewIfNotExist ::
       View a   -- ^ Select query from which the view is created.
    -> Create a
createViewIfNotExist = CreateView True

-- | Create a DEFAULT value constraint.
defaultValue :: ToColRefs a [ColRef b c] => a -> ColConstraintType c
defaultValue e = Default $ expr e

-- | Create a DROP TABLE statement.
dropTable ::
       (ToTables a [Table b])
    => a      -- ^ Table to drop. 
    -> Drop b
dropTable = DropTable False . table

dropTableIfExists ::
       ToTables a [Table b]
    => a      -- ^ Table to drop. 
    -> Drop b
dropTableIfExists = DropTable True . table

-- | Create a DROP VIEW statement.
dropView ::
       View a -- ^ View to drop.
    -> Drop a
dropView = DropView False

-- | Create a DROP VIEW IF EXISTS statement.
dropViewIfExists ::
       View a -- ^ View to drop.
    -> Drop a
dropViewIfExists = DropView True

-- | Create a FOREIGN KEY constraint.
foreignKey ::
    ( ToTables a [Table d]
    , ToCols   b [Column c d]
    )
    => a -- ^ Table.
    -> b -- ^ Columns.
    -> ColConstraintType d
foreignKey t c =
    Reference $ ForeignKey (table t) (map ColWrap $ toCols c) Nothing Nothing

-- | Create a NOT NULL constraint.
notNull :: ColConstraintType a
notNull = NotNull

-- | Create a NULL constraint.
nullable :: ColConstraintType a
nullable = Null

-- | Create a PRIMARY KEY constraint.
primary ::
       Bool -- ^ If True, the primary key will be an AUTOINCREMENT.
    -> ColConstraintType a
primary = Primary

-- | Create a PRIMARY KEY constraint to be used in a table constraint.
primaryT :: ToCols a [Column b c] => a -> TableConstraintType c
primaryT c = TCPrimaryKey $ map ColWrap $ toCols c

-- | Create a table constraint.
tableConstraint :: String -> TableConstraintType a -> TableConstraint a
tableConstraint name constraintType =
    TableConstraint (maybeString name) constraintType Nothing

-- | Create an UNIQUE column constraint.
unique :: ColConstraintType a
unique = Unique

-- | Create an UNIQUE table constraint
uniqueT :: [ColWrap a] -> TableConstraintType a
uniqueT cs = TCUnique cs