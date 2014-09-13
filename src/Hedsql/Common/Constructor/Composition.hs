{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Hedsql/Common/Constructor/Tables.hs
Description : Queries and statements composition.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

All the composition of SQL queries and statements by adding their different
individual elements.
-}
module Hedsql.Common.Constructor.Composition
    (
      Add
    , (/++)
    ) where

import Hedsql.Common.Constructor.TablesManipulation
import Hedsql.Common.DataStructure.Base

import Control.Lens

-- private functions.

-- public functions.
{-|
Allow to easily add optional elements to data types using the '/++' infix
function.

For example, if you wish to add an ORDER BY clause to a SELECT query you can do
it as follow:
@
    selectQuery
/++ orderByClause
@
-}
class Add a b where
    (/++) ::
           a -- ^ Target.
        -> b -- ^ Element to add.
        -> a -- ^ Target returned with the added element.

-- | Add a LIMIT to an ORDER BY part.
instance Add OrderBy Limit where
    (/++) target el = set partOrderByLimit (Just el) target

-- | Add an OFFSET to an ORDER BY part.
instance Add OrderBy Offset where
    (/++) target el = set partOrderByOffset (Just el) target

-- | Add a table constraint to a CREATE TABLE statement.
instance Add CreateTable TableConstraint where
    (/++) target el = target /++ [el]

instance Add CreateTable [TableConstraint] where
    (/++) target el = set createTableConstraints (Just el) target

-- | Add a WHERE part to a DELETE query.
instance Add Delete Where where
    (/++) target el = set deleteWherePart (Just el) target

-- | Add a FROM part to a SELECT query.
instance Add SelectQuery From where
    (/++) target el = set fromClause (Just el) target

-- | Add a GROUP BY part to a SELECT query.
instance Add SelectQuery GroupBy where
    (/++) target el = set groupByClause (Just el) target

-- | Add an ORDER BY part to a SELECT query.
instance Add SelectQuery OrderBy where
    (/++) target el = set orderByClause (Just el) target

-- | Add a WHERE part to a SELECT query.
instance Add SelectQuery Where where
    (/++) target el = set whereClause (Just el) target

-- | Add a WHERE part to an UPDATE query.
instance Add Update Where where
    (/++) target el = set updateWherePart (Just el) target

-- | Add a Maybe value to a column.
instance Add Column a => Add Column (Maybe a) where
    (/++) target (Just el) = target /++ el
    (/++) target Nothing = target

-- | Add one constraint to a column.
instance Add Column ColConstraint where
    (/++) target el = target /++ [el]

-- | Add constraints to a column.
instance Add Column [ColConstraint] where
    (/++) target el = set colConstraints (Just el) target

-- | Add a column constraint type to a column.
instance Add Column ColConstraintType where
    (/++) target el = target /++ colConstraint "" el

-- | Add many column constraints types to a column.
instance Add Column [ColConstraintType] where
    (/++) target els = target /++ map (colConstraint "") els

-- | Specify the SQL data type of a column.
instance Add Column SqlDataType where
    (/++) target el = set colDataType (Just el) target

{-|
Add a table to a column
(to indicate that this column belongs to a specific table).
-}
instance Add Column Table where
    (/++) target el = set colTable (Just el) target