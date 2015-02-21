{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Common/Constructor/Tables.hs
Description : Queries and statements composition.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

All the composition of SQL queries and statements by adding their different
individual elements.
-}
module Database.Hedsql.Common.Constructor.Composition
    ( (/++)
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.Constructor.TablesManipulation
import Database.Hedsql.Common.DataStructure hiding (Add)

import Control.Lens (ASetter, set)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Set a Maybe value to the target using the provide element.
setMaybe ::
       ASetter s t a (Maybe b) -- ^ Lens.
    -> s                       -- ^ Target.
    -> b                       -- ^ Element to set.
    -> t                       -- ^ Target with the provided element set.
setMaybe l target el = set l (Just el) target

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
    addElem ::
           a c -- ^ Target.
        -> b c -- ^ Element to add.
        -> a c -- ^ Target returned with the added element.

-- | Add one constraint to a column.
instance Add Column ColConstraint where
    addElem target el = setMaybe colConstraints target [el]

-- | Add constraints to a column.
instance Add Column ColConstraints where
    addElem cols (ColConstraints cs)= setMaybe colConstraints cols cs

-- | Add a column constraint type to a column.
instance Add Column ColConstraintType where
    addElem target el = addElem target $ colConstraint "" el

-- | Add a column constraint types to a column.
instance Add Column ColConstraintTypes where
    addElem target (ColConstraintTypes els) =
        addElem target $ ColConstraints $ map (colConstraint "") els

-- | Specify the SQL data type of a column.
instance Add Column SqlDataType where
    addElem = setMaybe colDataType

-- | Add a WHERE part to a DELETE query.
instance Add Delete Where where
    addElem = setMaybe deleteWhere

-- | Add a HAVING clause to a GROUP BY clause.
instance Add GroupBy Having where
    addElem = setMaybe groupByHaving

-- | Add a LIMIT to an ORDER BY part.
instance Add OrderBy Limit where
    addElem = setMaybe partOrderByLimit

-- | Add an OFFSET to an ORDER BY part.
instance Add OrderBy Offset where
    addElem = setMaybe partOrderByOffset

-- | Add a FROM part to a SELECT query.
instance Add Select From where
    addElem = setMaybe fromClause

-- | Add a GROUP BY part to a SELECT query.
instance Add Select GroupBy where
    addElem = setMaybe groupByClause

-- | Add an ORDER BY part to a SELECT query.
instance Add Select OrderBy where
    addElem = setMaybe orderByClause

-- | Add a WHERE part to a SELECT query.
instance Add Select Where where
    addElem = setMaybe whereClause

-- | Add a table constraint to a CREATE TABLE statement.
instance Add Table TableConstraint where
    addElem target el = setMaybe tableConstraints target [el]

-- | Add a WHERE part to an UPDATE query.
instance Add Update Where where
    addElem = setMaybe updateWherePart

{-|
Coerce a type to another type which can then be used by an Add instance.

This is a hack to allow the use of lists by instances of the Add class since
such instances can only work on phantom types of kind * -> *.

Thus lists are converted to newtypes and the instances for the other types
are just aliases of the id function.
-}
class ToAddable a b | a -> b where
    toConvertible :: a -> b

instance ToAddable (ColConstraint a) (ColConstraint a) where
    toConvertible = id

newtype ColConstraints a = ColConstraints [ColConstraint a]

instance ToAddable [ColConstraint a] (ColConstraints a) where
    toConvertible = ColConstraints

instance ToAddable (ColConstraintType a) (ColConstraintType a) where
    toConvertible = id

newtype ColConstraintTypes a = ColConstraintTypes [ColConstraintType a]

instance ToAddable [ColConstraintType a] (ColConstraintTypes a) where
    toConvertible = ColConstraintTypes

instance ToAddable (From a) (From a) where
    toConvertible = id

instance ToAddable (GroupBy a) (GroupBy a) where
    toConvertible = id

instance ToAddable (Having a) (Having a) where
    toConvertible = id

instance ToAddable (Limit a) (Limit a) where
    toConvertible = id

instance ToAddable (Offset a) (Offset a) where
    toConvertible = id

instance ToAddable (OrderBy a) (OrderBy a) where
    toConvertible = id

instance ToAddable (SqlDataType a) (SqlDataType a) where
    toConvertible = id

instance ToAddable (Table a) (Table a) where
    toConvertible = id

instance ToAddable (TableConstraint a) (TableConstraint a) where
    toConvertible = id

instance ToAddable (Where a) (Where a) where
    toConvertible = id

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Allow to easily add optional elements to data types using the '/++' infix
function.

For example, if you wish to add an ORDER BY clause to a SELECT query you can do
it as follow:
> selectQuery /++ orderByClause
-}
(/++) :: (Add a d, ToAddable b (d c)) => a c -> b -> a c
(/++) target element = addElem target (toConvertible element)