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
    ( check
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
    
import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.Constructor.Conditions
import Database.Hedsql.Common.Constructor.Tables
import Database.Hedsql.Common.DataStructure

import Control.Lens
import Prelude      hiding (null)

-- TODO: implement ALTER statements.

-- private functions.

-- | Return nothing if the provided string is empty.
maybeString :: String -> Maybe String
maybeString ""   = Nothing
maybeString name = Just name

-- public functions.

-- | Create a CHECK constraint.
check :: ToConditions (a b) [Condition b] => a b -> ColConstraintType b
check cond = Check $ condition cond

-- | Create a CHECK constraint to be used in a table constraint.
checkT :: ToConditions (a b) [Condition b] => a b -> TableConstraintType b
checkT = TableConstraintCheck . condition

-- | Create a constraint which shall then be applied on a column.
colConstraint :: String -> ColConstraintType a -> ColConstraint a
colConstraint name = ColConstraint (maybeString name)

-- | Create a CREATE TABLE statement.
createTable :: ToTables a [Table b] => a -> [Column b] -> Table b
createTable t c = table t & tableCols .~ c

-- | Create a CREATE TABLE IF NOT EXIST statement.
createTableIfNotExist :: ToTables a [Table b] => a -> [Column b] -> Table b
createTableIfNotExist t c =
    table t
        & tableCols .~ c
        & tableIfNotExists .~ True

-- | Create a CREATE VIEW query.
createView ::
       String       -- ^ Name of the view.
    -> Select a     -- ^ Select query from which the view is created.
    -> CreateView a
createView = CreateView

-- | Create a DEFAULT value constraint.
defaultValue :: ToColRefs a [ColRef b] => a -> ColConstraintType b
defaultValue e = Default $ expr e

-- | Create a DROP TABLE statement.
dropTable ::
       (ToTables a [Table b])
    => a                       -- ^ Table to drop. 
    -> DropTable b
dropTable = DropTable False . table

dropTableIfExists ::
       ToTables a [Table b]
    => a -- ^ Table or name of the table.
    -> DropTable b
dropTableIfExists name = DropTable True $ table name

-- | Create a DROP VIEW query.
dropView ::
       String      -- ^ Name of the view.
    -> DropView a
dropView = DropView

-- | Create a FOREIGN KEY constraint.
foreignKey ::
    ( ToTables a [Table c]
    , ToCols   b [Column c]
    )
    => a -- ^ Table.
    -> b -- ^ Column.
    -> ColConstraintType c
foreignKey t c = Reference (table t) (toCol c) Nothing

-- | Create a NOT NULL constraint.
notNull :: ColConstraintType a
notNull = NotNull

-- | Create a NULL constraint.
null :: ColConstraintType a
null = Null

-- | Create a NULL value.
nullVal :: SqlValue a
nullVal = SqlValueNull

-- | Create a PRIMARY KEY constraint.
primary ::
       Bool -- ^ If True, the primary key will be an AUTOINCREMENT.
    -> ColConstraintType a
primary = Primary

-- | Create a PRIMARY KEY constraint to be used in a table constraint.
primaryT :: ToCols a [Column b] => a -> TableConstraintType b
primaryT = TableConstraintPrimaryKey . toCols

-- | Create a table constraint.
tableConstraint :: String -> TableConstraintType a -> TableConstraint a
tableConstraint name constraintType =
    TableConstraint (maybeString name) constraintType Nothing

-- | Create an UNIQUE column constraint.
unique :: ColConstraintType a
unique = Unique

-- | Create an UNIQUE table constraint
uniqueT :: ToCols a [Column b] => a -> TableConstraintType b
uniqueT = TableConstraintUnique . toCols