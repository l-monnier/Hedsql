{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Common/Constructor/Tables.hs
Description : Constructor functions for tables.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for table references which can then be used in queries.
-}
module Database.Hedsql.Common.Constructor.Tables
    ( ToTables
    , ToTableRefs
    , alias
    , toTable
    , toTables
    , table
    , tables
    , tableRef
    , tableRefs
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.Constructor.Types
import Database.Hedsql.Common.DataStructure

--------------------------------------------------------------------------------
-- Public
--------------------------------------------------------------------------------

-- | Coerce a given type to a list of tables.
class ToTables a b | a -> b where
    toTables :: a -> b

-- | Create a table from its name.
instance ToTables (SqlString a) [Table a] where
    toTables name = [Table False name [] Nothing]

-- | Create a table from itself.
instance ToTables (Table a) [Table a] where
    toTables t = [t]

{-|
Convert a table reference to a list of tables.
If the table reference is a table, then use that table.
Else, create a table using the name of this table reference.
-}
instance ToTables (TableRef a) [Table a] where
    toTables ref =
        case ref of
            TableTableRef t _ -> [t]
            _                 -> [Table False (getTableRefName ref) [] Nothing]

-- | Coerce a given type to a list of TableRef.
class ToTableRefs a b | a -> b where
    toTablesRef :: a -> b

instance ToTableRefs (Join a) [TableRef a] where
    toTablesRef join = [TableJoinRef join Nothing]

instance ToTableRefs (SqlString a) [TableRef a] where
    toTablesRef name = [TableTableRef (toTable name) Nothing]

instance ToTableRefs (Table a) [TableRef a] where
    toTablesRef name = [TableTableRef name Nothing]

instance ToTableRefs (TableRef a) [TableRef a] where
    toTablesRef ref = [ref]

instance ToTableRefs [SqlString a] [TableRef a] where
    toTablesRef = map (head.toTablesRef)
    
instance ToTableRefs [Table a] [TableRef a] where
    toTablesRef = map (head.toTablesRef)
    
instance ToTableRefs [TableRef a] [TableRef a] where
    toTablesRef = id

-- | Create a table alias using AS.
alias :: ToTableRefs a [TableRef b] => a -> String -> TableRef b
alias t name =
    setAlias ref
    where
        al  = TableRefAs name []
        ref = tableRef t
        setAlias (LateralTableRef a _) = LateralTableRef a al
        setAlias (SelectTableRef  a _) = SelectTableRef  a al
        setAlias (TableJoinRef    a _) = TableJoinRef    a (Just al)
        setAlias (TableTableRef   a _) = TableTableRef   a (Just al)

-- | Create a table which can then be used as so in a query.
table ::
       ToTables a [Table b]
    => a -- ^ The table itself or its name as a string.
    -> Table b
table = toTable

-- | Create a table reference which can then be used in a query.
tableRef :: ToTableRefs a [TableRef b] => a -> TableRef b
tableRef = head.toTablesRef

-- | Create many table reference which can then be used in a query.
tableRefs :: ToTableRefs a [TableRef b] => a -> [TableRef b]
tableRefs = toTablesRef

-- | Create many tables which can then be used as so in a query.
tables ::
       ToTables a [Table b]
    => [a] -- ^ The tables themselves or their name as a string.
    -> [Table b]
tables = map table

-- | Coerce a given type to a table.
toTable :: ToTables a [Table b] => a -> Table b
toTable = head.toTables