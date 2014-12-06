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
    (/++)
    ) where

import Hedsql.Common.Constructor.TablesManipulation
import Hedsql.Common.DataStructure.Base hiding (Add)

import qualified Data.Coerce as C

import Control.Lens

-- private functions.

-- | Set a Maybe value to the target using the provide element and coercing it.
setMaybe :: C.Coercible b c => ASetter s t a (Maybe c) -> s -> b -> t
setMaybe lens target el = set lens (Just $ C.coerce el) target

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

instance Add (GroupBy a) (Having b) where
    (/++) = setMaybe groupByHaving

-- | Add a LIMIT to an ORDER BY part.
instance Add (OrderBy a) (Limit b) where
    (/++) = setMaybe partOrderByLimit

-- | Add an OFFSET to an ORDER BY part.
instance Add (OrderBy a) (Offset b) where
    (/++) = setMaybe partOrderByOffset

-- | Add a table constraint to a CREATE TABLE statement.
instance Add (CreateTable a) (TableConstraint b) where
    (/++) target el = target /++ [el]

instance Add (CreateTable a) [TableConstraint b] where
    (/++) = setMaybe createTableConstraints

-- | Add a WHERE part to a DELETE query.
instance Add (Delete a) (Where b) where
    (/++) = setMaybe deleteWhere

-- | Add a FROM part to a SELECT query.
instance Add (Select a) (From b) where
    (/++) = setMaybe fromClause

-- | Add a GROUP BY part to a SELECT query.
instance Add (Select a) (GroupBy b) where
    (/++) = setMaybe groupByClause

-- | Add an ORDER BY part to a SELECT query.
instance Add (Select a) (OrderBy b) where
    (/++) = setMaybe orderByClause

-- | Add a WHERE part to a SELECT query.
instance Add (Select a) (Where b) where
    (/++) = setMaybe whereClause

-- | Add a WHERE part to an UPDATE query.
instance Add (Update a) (Where b) where
    (/++) = setMaybe updateWherePart

-- | Add a Maybe value to a column.
instance Add (Column a) b => Add (Column a) (Maybe b) where
    (/++) target (Just el) = target /++ el
    (/++) target Nothing = target

-- | Add one constraint to a column.
instance Add (Column a) (ColConstraint b) where
    (/++) target el = target /++ [el]

-- | Add constraints to a column.
instance Add (Column a) [ColConstraint b] where
    (/++) = setMaybe colConstraints

-- | Add a column constraint type to a column.
instance Add (Column a) (ColConstraintType b) where
    (/++) target el = target /++ colConstraint "" el

-- | Add many column constraints types to a column.
instance Add (Column a) [ColConstraintType b] where
    (/++) target els = target /++ map (colConstraint "") els
    
-- | Specify the SQL data type of a column.
instance Add (Column a) SqlDataType where
    (/++) target el = set colDataType (Just el) target

{-|
Add a table to a column
(to indicate that this column belongs to a specific table).
-}
instance Add (Column a) (Table b) where
    (/++) = setMaybe colTable