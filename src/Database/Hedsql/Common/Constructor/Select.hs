{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoOverloadedStrings    #-}

{-|
Module      : Database/Hedsql/Common/Constructor/Queries.hs
Description : Constructor functions for SQL SELECT queries.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for SQL SELCT queries.
-}
module Database.Hedsql.Common.Constructor.Select
    ( (//*)
    , alias
    , asc
    , combinedQuery
    , condition
    , crossJoin
    , desc
    , except
    , exceptAll
    , from
    , fullJoin
    , groupBy
    , having
    , innerJoin
    , intersect
    , intersectAll
    , isDistinctFrom
    , isNotDistinctFrom
    , leftJoin
    , limit
    , naturalFullJoin
    , naturalLeftJoin
    , naturalInnerJoin
    , naturalRightJoin
    , nullsFirst
    , nullsLast
    , offset
    , orderBy
    , rightJoin
    , select
    , selectDistinct
    , sortRef
    , sortRefs
    , subQuery
    , union
    , unionAll
    , where_
    ) where
    
import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.Constructor.Conditions
import Database.Hedsql.Common.Constructor.Functions()
import Database.Hedsql.Common.Constructor.Tables
import Database.Hedsql.Common.Constructor.Types
import Database.Hedsql.Common.DataStructure

import Control.Lens hiding (from)

-- private functions.

-- | Coerce a type to a CombinedQuery type.
class ToCombined a b where
    toCombined :: a c -> b c

instance ToCombined CombinedQuery CombinedQuery where
    toCombined = id
 
instance ToCombined Select CombinedQuery where
    toCombined = CombinedQuerySingle

-- | Coerce a type to a list of JoinClause type.
class ToJoinClauses a b | a -> b where
    toJoinClauses :: a -> b

-- | Create an ON join clause from a predicate.
instance ToJoinClauses (Condition a) [JoinClause a] where
    toJoinClauses = list . JoinClauseOn
    
-- | Create an ON join clause from a boolean function.
instance ToJoinClauses (FuncBool a) [JoinClause a] where
    toJoinClauses = list . JoinClauseOn . FuncCond

-- | Create an USING joint clause from a column.
instance ToJoinClauses (Column a) [JoinClause a] where
    toJoinClauses c = list $ JoinClauseUsing [c]

-- | Create an USING join clause from a list of columns.
instance ToJoinClauses [Column a] [JoinClause a] where
    toJoinClauses = list . JoinClauseUsing

-- | Create an USING join clause from a string which is a column name.
instance ToJoinClauses (SqlString a) [JoinClause a] where
    toJoinClauses = list . JoinClauseUsing . toCols

-- | Create an USING join clause from a list of strings which are column names.    
instance ToJoinClauses [SqlString a] [JoinClause a] where
    toJoinClauses = list . JoinClauseUsing . toCols

-- TODO: check if used at all.
--instance ToJoinClauses (Join a) (TableRef a) where
--    toJoinClauses join = list $ TableJoinRef join Nothing

-- | Coerce a type to a list of SortRef types.
class ToSortRefs a b | a -> b where
    toSortRefs :: a -> b

instance ToSortRefs (ColRef a) [SortRef a] where
    toSortRefs ref = [SortRef ref Nothing Nothing]

instance ToSortRefs (SqlString a) [SortRef a] where
    toSortRefs name = [SortRef (colRef name) Nothing Nothing]

instance ToSortRefs (SortRef a) [SortRef a] where
    toSortRefs ref = [ref]

-- | Create a join on columns with a USING or ON clause.
columnJoin ::
   (
      ToTableRefs   a [TableRef   d]
   ,  ToTableRefs   b [TableRef   d]
   ,  ToJoinClauses c [JoinClause d]
   )
   => JoinTypeCol d
   -> a
   -> b
   -> c
   -> Join        d
columnJoin joinType tableRef1 tableRef2 clause =
    JoinColumn
         joinType
        (tableRef tableRef1)
        (tableRef tableRef2)
        (joinClause clause)

-- | Convert an element to a list with itself as the only item.
list :: a -> [a]
list a = [a]
 
-- | Create a join on tables (CROSS or NATURAL join).
tableJoin ::
       ToTableRefs   a [TableRef b]
    => JoinTypeTable b
    -> a
    -> a
    -> Join b
tableJoin joinType tableRef1 tableRef2 =
    JoinTable
        joinType
        (tableRef tableRef1)
        (tableRef tableRef2)

-- | Convert a type to a join clause.
toJoinClause :: ToJoinClauses a [JoinClause b] => a -> JoinClause b
toJoinClause = head.toJoinClauses

-- public functions.

-- | Create a combined query such as an INTERSECT, EXCEPT, etc.
combinedQuery ::
    ToCombined a CombinedQuery => a b -> CombinedQuery b
combinedQuery = toCombined
   
-- | Create a JOIN clause such as ON or USING.
joinClause :: ToJoinClauses a [JoinClause b] => a -> JoinClause b
joinClause = toJoinClause

{-|
Convert a value to a sorting reference: a reference which can be used in an
ORDER BY clause.
-}
sortRef :: ToSortRefs a [SortRef b] => a -> SortRef b
sortRef = head.toSortRefs

sortRefs :: ToSortRefs a [SortRef b] => a -> [SortRef b]
sortRefs = toSortRefs

-- | Create a joker - "*" - character.
(//*) :: Function a
(//*) = Joker

{-|
Add an ascending sorting order (ASC) to a sort reference
(which can be a column reference).
-}
asc :: ToSortRefs a [SortRef b] => a -> SortRef b
asc ref =  set sortRefOrder (Just Asc) (sortRef ref)

-- | Create a CROSS JOIN.
crossJoin :: ToTableRefs a [TableRef b] => a -> a -> Join b
crossJoin = tableJoin CrossJoin

-- | Apply an EXCEPT to two queries.
except ::
    ToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
except c1 c2 = CombinedQueryExcept [combinedQuery c1, combinedQuery c2]

-- | Apply an EXCEPT ALL to two queries.
exceptAll ::
    ToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
exceptAll c1 c2 =
    CombinedQueryExceptAll [combinedQuery c1, combinedQuery c2]

{-|
Add a descending sorting order (DESC) to a sort reference
(which can be a column reference).
-}
desc :: ToSortRefs a [SortRef b] => a -> SortRef b
desc ref =  set sortRefOrder (Just Desc) (sortRef ref)

-- | Add a FROM clause to a SELECT query.
from :: ToTableRefs a [TableRef b] => a -> From b
from = From . tableRefs

-- | Create a FULL JOIN.
fullJoin ::
    ( ToTableRefs   a [TableRef   d]
    , ToTableRefs   b [TableRef   d]
    , ToJoinClauses c [JoinClause d]
    )
    => a      -- ^ First table reference.
    -> b      -- ^ Second table reference.
    -> c      -- ^ Join clause.
    -> Join d
fullJoin = columnJoin FullJoin

-- | Create a GROUP BY clause.
groupBy :: ToColRefs a [ColRef b] => a -> GroupBy b
groupBy cs = GroupBy (colRefs cs) Nothing

-- | Add a HAVING clause to a GROUP BY clause.
having :: ToConditions a [Condition b] => a -> Having b
having = Having . condition

-- | Create a IS DISTINCT FROM operator.
isDistinctFrom ::
    ( ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
isDistinctFrom colRef1 colRef2 =
    IsDistinctFrom (colRef colRef1) (colRef colRef2)

-- | Create a IS NOT DISTINCT FROM operator.
isNotDistinctFrom ::
    ( ToColRefs a [ColRef c]
    , ToColRefs b [ColRef c]
    )
    => a
    -> b
    -> FuncBool c
isNotDistinctFrom colRef1 colRef2 =
    IsNotDistinctFrom (colRef colRef1) (colRef colRef2)

{-|
Create an INNER JOIN.
If the join clause is a condition or a boolean function, it will be an ON
clause.
If the join clause is a column, a string or a list of columns or strings, it
will be an USING clause.
-} 
innerJoin ::
    ( ToTableRefs   a [TableRef   d]
    , ToTableRefs   b [TableRef   d]
    , ToJoinClauses c [JoinClause d]
    )
    => a      -- ^ First table reference.
    -> b      -- ^ Second table reference.
    -> c      -- ^ Join clause.
    -> Join d
innerJoin = columnJoin InnerJoin

-- | Apply an INTERSECT to two queries.
intersect ::
    ToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
intersect c1 c2 =
    CombinedQueryIntersect [combinedQuery c1, combinedQuery c2]

-- | Apply an INTERSECT ALL to two queries.
intersectAll ::
    ToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
intersectAll c1 c2 =
    CombinedQueryIntersectAll [combinedQuery c1, combinedQuery c2]

-- | Create a LEFT JOIN.
leftJoin ::
    ( ToTableRefs   a [TableRef   d]
    , ToTableRefs   b [TableRef   d]
    , ToJoinClauses c [JoinClause d]
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> c -- ^ Join clause.
    -> Join d
leftJoin = columnJoin LeftJoin

-- | Add a LIMIT clause to an ORDER BY part.
limit :: Int -> Limit a
limit = Limit

-- | Create a NATURAL FULL JOIN.
naturalFullJoin :: ToTableRefs a [TableRef b] => a -> a -> Join b
naturalFullJoin = tableJoin NaturalFullJoin

-- | Create a NATURAL LEFT JOIN.
naturalLeftJoin :: ToTableRefs a [TableRef b] => a -> a -> Join b
naturalLeftJoin = tableJoin NaturalLeftJoin

-- | Create a NATURAL INNER JOIN.
naturalInnerJoin :: ToTableRefs a [TableRef b] => a -> a -> Join b
naturalInnerJoin = tableJoin NaturalInnerJoin

-- | Create a NATURAL RIGHT JOIN.
naturalRightJoin :: ToTableRefs a [TableRef b] => a -> a -> Join b
naturalRightJoin = tableJoin NaturalRightJoin

{-|
Add a nulls first option (NULLS FIRST) to a sort reference
(which can be a column reference).
-}
nullsFirst :: ToSortRefs a [SortRef b] => a -> SortRef b
nullsFirst sRef =  set sortRefNulls (Just NullsFirst) (sortRef sRef)

{-|
Add a nulls last option (NULLS LAST) to a sort reference
(which can be a column reference).
-}
nullsLast:: ToSortRefs a [SortRef b] => a -> SortRef b
nullsLast sRef =  set sortRefNulls (Just NullsLast) (sortRef sRef)

-- | Create an OFFSET clause.
offset :: Int -> Offset a
offset = Offset

-- | Add an ORDER BY clause to a query.
orderBy ::
       ToSortRefs a [SortRef b]
    => a          -- ^ Sorting references.
    -> OrderBy b
orderBy cs = OrderBy (sortRefs cs) Nothing Nothing

-- | Create a RIGHT JOIN.
rightJoin ::
    ( ToTableRefs   a [TableRef d]
    , ToTableRefs   b [TableRef d]
    , ToJoinClauses c [JoinClause d]
    )
    => a      -- ^ First table reference.
    -> b      -- ^ Second table reference.
    -> c      -- ^ Join clause.
    -> Join d
rightJoin = columnJoin RightJoin

-- | Create a SELECT query.
select :: ToColRefs a [ColRef b] => a -> Select b
select a =
    Select
        (colRefs a)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

-- | Create a SELECT DISTINCT query.
selectDistinct :: ToColRefs a [ColRef b] => a -> Select b   
selectDistinct a =
    Select
        (colRefs a)
        (Just Distinct)
        Nothing
        Nothing
        Nothing
        Nothing

-- | Create a sub-query in a FROM clause.
subQuery ::
       Select   a -- ^ Sub-query.
    -> String     -- ^ Alias of the sub-query.
    -> TableRef a -- ^ Table reference.
subQuery sub name = SelectTableRef sub $ TableRefAs name []

-- | Create an UNION operation between two queries.
union ::
       ToCombined a CombinedQuery
    => a b
    -> a b
    -> CombinedQuery b
union c1 c2 = CombinedQueryUnion [combinedQuery c1, combinedQuery c2]

-- | Create an UNION ALL operation between two queries.
unionAll ::
       ToCombined a CombinedQuery
    => a             b
    -> a             b
    -> CombinedQuery b
unionAll c1 c2 = CombinedQueryUnionAll [combinedQuery c1, combinedQuery c2]

-- | Create a WHERE clause for a SELECT query.
where_ :: ToConditions a [Condition b] => a -> Where b
where_ = Where . condition