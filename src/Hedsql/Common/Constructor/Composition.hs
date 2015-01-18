{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

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
    ( (/++)
    ) where

import Hedsql.Common.Constructor.TablesManipulation
import Hedsql.Common.DataStructure hiding (Add)

import Control.Lens hiding (coerce)
import Data.Coerce

-- private functions.

-- | Set a Maybe value to the target using the provide element.
setMaybe :: ASetter s t a (Maybe b) -> s -> b -> t
setMaybe lens target el = set lens (Just el) target

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
           a c -- ^ Target.
        -> b c -- ^ Element to add.
        -> a c -- ^ Target returned with the added element.

instance Add GroupBy Having where
    (/++) = setMaybe groupByHaving

-- | Add a LIMIT to an ORDER BY part.
instance Add OrderBy Limit where
    (/++) = setMaybe partOrderByLimit

-- | Add an OFFSET to an ORDER BY part.
instance Add OrderBy Offset where
    (/++) = setMaybe partOrderByOffset

-- | Add a table constraint to a CREATE TABLE statement.
instance Add CreateTable TableConstraint where
    (/++) target el = setMaybe createTableConstraints target [el]

--instance Add CreateTable TableConstraint where
--    (/++) = setMaybe createTableConstraints

-- | Add a WHERE part to a DELETE query.
instance Add Delete Where where
    (/++) = setMaybe deleteWhere

-- | Add a FROM part to a SELECT query.
instance Add Select From where
    (/++) = setMaybe fromClause

-- | Add a GROUP BY part to a SELECT query.
instance Add Select GroupBy where
    (/++) = setMaybe groupByClause

-- | Add an ORDER BY part to a SELECT query.
instance Add Select OrderBy where
    (/++) = setMaybe orderByClause

-- | Add a WHERE part to a SELECT query.
instance Add Select Where where
    (/++) = setMaybe whereClause

-- | Add a WHERE part to an UPDATE query.
instance Add Update Where where
    (/++) = setMaybe updateWherePart

-- | Add one constraint to a column.
instance Add Column ColConstraint where
    (/++) target el = setMaybe colConstraints target [el]

-- | Add constraints to a column.
--instance Add (Column a) [ColConstraint a] where
--    (/++) = setMaybe colConstraints

-- | Add a column constraint type to a column.
instance Add Column ColConstraintType where
    (/++) target el = target /++ colConstraint "" el

-- | Add many column constraints types to a column.
--instance Add (Column a) [ColConstraintType a] where
--    (/++) target els = target /++ map (colConstraint "") els
    
-- | Specify the SQL data type of a column.
instance Add Column SqlDataType where
    (/++) = setMaybe colDataType

{-|
Add a table to a column
(to indicate that this column belongs to a specific table).
-}
instance Add Column Table where
    (/++) = setMaybe colTable