{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoOverloadedStrings    #-}

{-|
Module      : Hedsql/Common/Constructor/Queries.hs
Description : Constructor functions for SQL SELECT queries.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for SQL SELCT queries.
-}
module Hedsql.Common.Constructor.Select
    (
      (//*)
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
    , selectDistinctOn
    , sortRef
    , sortRefs
    , subQuery
    , union
    , unionAll
    , where_
    ) where

import Hedsql.Common.Constructor.Columns
import Hedsql.Common.Constructor.Composition
import Hedsql.Common.Constructor.Conditions
import Hedsql.Common.Constructor.Tables
import Hedsql.Common.Constructor.Types
import Hedsql.Common.DataStructure

import Control.Lens hiding (from)

import Hedsql.Common.Constructor.Functions

-- private functions.

-- | Coerce a type to a CombinedQuery type.
class CoerceToCombined a b where
    coerceToCombined :: a c -> b c

instance CoerceToCombined CombinedQuery CombinedQuery where
    coerceToCombined = id
 
instance CoerceToCombined Select CombinedQuery where
    coerceToCombined = CombinedQuerySingle

-- | Coerce a type to a JoinClause type.
class CoerceToJoinClause a b | a -> b where
    coerceToJoinClause :: a -> b

-- | Create an ON join clause from a predicate.
instance CoerceToJoinClause (Condition a) (JoinClause a) where
    coerceToJoinClause = JoinClauseOn
    
-- | Create an ON join clause from a boolean function.
instance CoerceToJoinClause (FuncBool a) (JoinClause a) where
    coerceToJoinClause = JoinClauseOn . FuncCond

instance CoerceToJoinClause (Column a) (JoinClause a) where
    coerceToJoinClause col = JoinClauseUsing [col]

-- | Create an USING join clause from a list of columns.
instance CoerceToJoinClause [Column a] (JoinClause a) where
    coerceToJoinClause = JoinClauseUsing

-- | Create an USING join clause from a string which is a column name.
instance CoerceToJoinClause (SqlString a) (JoinClause a) where
    coerceToJoinClause col = JoinClauseUsing [column col]
    
instance CoerceToJoinClause [SqlString a] (JoinClause a) where
    coerceToJoinClause cols = JoinClauseUsing (columns cols)

instance CoerceToJoinClause (Join a) (TableRef a) where
    coerceToJoinClause join = TableJoinRef join Nothing

-- | Coerce a type to a list of SortRef types.
class CoerceToSortRef a b | a -> b where
    coerceToSortRef :: a -> b

instance CoerceToSortRef (ColRef a) [SortRef a] where
    coerceToSortRef ref = [SortRef ref Nothing Nothing]

instance CoerceToSortRef (SqlString a) [SortRef a] where
    coerceToSortRef name = [SortRef (colRef name) Nothing Nothing]

instance CoerceToSortRef (SortRef a) [SortRef a] where
    coerceToSortRef ref = [ref]

-- | Create a join on columns with a USING or ON clause.
columnJoin ::
   (
      CoerceToTableRef   a [TableRef d]
   ,  CoerceToTableRef   b [TableRef d]
   ,  CoerceToJoinClause c (JoinClause d))
   => JoinTypeCol d -> a -> b -> c -> Join d
columnJoin joinType tableRef1 tableRef2 clause =
    JoinColumn
         joinType
        (tableRef tableRef1)
        (tableRef tableRef2)
        (joinClause clause)
 
-- | Create a join on tables (CROSS or NATURAL join).
tableJoin ::
       CoerceToTableRef a [TableRef b]
    => JoinTypeTable b -> a -> a -> Join b
tableJoin joinType tableRef1 tableRef2 =
    JoinTable
        joinType
        (tableRef tableRef1)
        (tableRef tableRef2)

-- public functions.

-- | Create a combined query such as an INTERSECT, EXCEPT, etc.
combinedQuery ::
    CoerceToCombined a CombinedQuery => a b -> CombinedQuery b
combinedQuery = coerceToCombined
   
-- | Create a JOIN clause such as ON or USING.
joinClause :: CoerceToJoinClause a (JoinClause b) => a -> JoinClause b
joinClause = coerceToJoinClause

{-|
Convert a value to a sorting reference: a reference which can be used in an
ORDER BY clause.
-}
sortRef :: CoerceToSortRef a [SortRef b] => a -> SortRef b
sortRef = head.coerceToSortRef

sortRefs :: CoerceToSortRef a [SortRef b] => a -> [SortRef b]
sortRefs = coerceToSortRef

-- | Create a joker - "*" - character.
(//*) :: Function a
(//*) = JokerF Joker

{-|
Add an ascending sorting order (ASC) to a sort reference
(which can be a column reference).
-}
asc :: CoerceToSortRef a [SortRef b] => a -> SortRef b
asc ref =  set sortRefOrder (Just Asc) (sortRef ref)

-- | Create a CROSS JOIN.
crossJoin :: CoerceToTableRef a [TableRef b] => a -> a -> Join b
crossJoin = tableJoin CrossJoin

-- | Apply an EXCEPT to two queries.
except ::
    CoerceToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
except c1 c2 = CombinedQueryExcept [combinedQuery c1, combinedQuery c2]

-- | Apply an EXCEPT ALL to two queries.
exceptAll ::
    CoerceToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
exceptAll c1 c2 =
    CombinedQueryExceptAll [combinedQuery c1, combinedQuery c2]

{-|
Add a descending sorting order (DESC) to a sort reference
(which can be a column reference).
-}
desc :: CoerceToSortRef a [SortRef b] => a -> SortRef b
desc ref =  set sortRefOrder (Just Desc) (sortRef ref)

-- | Add a FROM clause to a SELECT query.
from :: CoerceToTableRef a [TableRef b] => a -> From b
from = From . tableRefs

-- | Create a FULL JOIN.
fullJoin ::
    (
      CoerceToTableRef   a [TableRef d]
    , CoerceToTableRef   b [TableRef d]
    , CoerceToJoinClause c (JoinClause d)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> c -- ^ Join clause.
    -> Join d
fullJoin = columnJoin FullJoin

-- | Create a GROUP BY clause.
groupBy :: CoerceToColRef a [ColRef b] => a -> GroupBy b
groupBy cols = GroupBy (colRefs cols) Nothing

-- | Add a HAVING clause to a GROUP BY clause.
having :: CoerceToCondition a (Condition b) => a -> Having b
having = Having . condition

-- | Create a IS DISTINCT FROM operator.
isDistinctFrom ::
      (CoerceToColRef a [ColRef c], CoerceToColRef b [ColRef c])
    => a -> b -> IsDistinctFrom c
isDistinctFrom colRef1 colRef2 =
    IsDistinctFrom (colRef colRef1) (colRef colRef2)

-- | Create a IS NOT DISTINCT FROM operator.
isNotDistinctFrom
  :: (CoerceToColRef a [ColRef c], CoerceToColRef b [ColRef c]) =>
     a -> b -> IsNotDistinctFrom c
isNotDistinctFrom colRef1 colRef2 =
    IsNotDistinctFrom (colRef colRef1) (colRef colRef2)

-- | Create a INNER JOIN.
innerJoin ::
    (
      CoerceToTableRef   a [TableRef d]
    , CoerceToTableRef   b [TableRef d]
    , CoerceToJoinClause c (JoinClause d)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> c -- ^ Join clause.
    -> Join d
innerJoin = columnJoin InnerJoin

-- | Apply an INTERSECT to two queries.
intersect ::
    CoerceToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
intersect c1 c2 =
    CombinedQueryIntersect [combinedQuery c1, combinedQuery c2]

-- | Apply an INTERSECT ALL to two queries.
intersectAll ::
    CoerceToCombined a CombinedQuery => a b -> a b -> CombinedQuery b
intersectAll c1 c2 =
    CombinedQueryIntersectAll [combinedQuery c1, combinedQuery c2]

-- | Create a LEFT JOIN.
leftJoin ::
    (
      CoerceToTableRef   a [TableRef d]
    , CoerceToTableRef   b [TableRef d]
    , CoerceToJoinClause c (JoinClause d)
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
naturalFullJoin :: CoerceToTableRef a [TableRef b] => a -> a -> Join b
naturalFullJoin = tableJoin NaturalFullJoin

-- | Create a NATURAL LEFT JOIN.
naturalLeftJoin :: CoerceToTableRef a [TableRef b] => a -> a -> Join b
naturalLeftJoin = tableJoin NaturalLeftJoin

-- | Create a NATURAL INNER JOIN.
naturalInnerJoin :: CoerceToTableRef a [TableRef b] => a -> a -> Join b
naturalInnerJoin = tableJoin NaturalInnerJoin

-- | Create a NATURAL RIGHT JOIN.
naturalRightJoin :: CoerceToTableRef a [TableRef b] => a -> a -> Join b
naturalRightJoin = tableJoin NaturalRightJoin

{-|
Add a nulls first option (NULLS FIRST) to a sort reference
(which can be a column reference).
-}
nullsFirst :: CoerceToSortRef a [SortRef b] => a -> SortRef b
nullsFirst sRef =  set sortRefNulls (Just NullsFirst) (sortRef sRef)

{-|
Add a nulls last option (NULLS LAST) to a sort reference
(which can be a column reference).
-}
nullsLast:: CoerceToSortRef a [SortRef b] => a -> SortRef b
nullsLast sRef =  set sortRefNulls (Just NullsLast) (sortRef sRef)

-- | Create an OFFSET clause.
offset :: Int -> Offset a
offset = Offset

-- | Add an ORDER BY clause to a query.
orderBy :: CoerceToSortRef a [SortRef b] => a -> OrderBy b
orderBy cols = OrderBy (sortRefs cols) Nothing Nothing

-- | Create a RIGHT JOIN.
rightJoin ::
    (
      CoerceToTableRef   a [TableRef d]
    , CoerceToTableRef   b [TableRef d]
    , CoerceToJoinClause c (JoinClause d)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> c -- ^ Join clause.
    -> Join d
rightJoin = columnJoin RightJoin

-- | Create a SELECT query.
select :: CoerceToColRef a [ColRef b] => a -> Select b
select a =
    Select
        (colRefs a)
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing

-- | Create a SELECT DISTINCT query.
selectDistinct :: CoerceToColRef a [ColRef b] => a -> Select b   
selectDistinct a =
    Select
        (colRefs a)
        (Just Distinct)
         Nothing
         Nothing
         Nothing
         Nothing

-- | Create a SELECT DISTINCT ON query.
selectDistinctOn ::
       (CoerceToColRef a [ColRef c], CoerceToColRef b [ColRef c])
    => [a]       -- ^ Distinct references.
    ->  b        -- ^ Select clause.
    ->  Select c -- ^ Select query.
selectDistinctOn distinctExpr selectExpr =
    Select
        (colRefs selectExpr)
        (Just $ DistinctOn $ exprs distinctExpr)
         Nothing
         Nothing
         Nothing
         Nothing

-- | Create a sub-query in a FROM clause.
subQuery ::
       Select a   -- ^ Sub-query.
    -> String     -- ^ Alias of the sub-query.
    -> TableRef a -- ^ Table reference.
subQuery sub name = SelectTableRef sub $ TableRefAs name []

-- | Create an UNION operation between two queries.
union ::
       CoerceToCombined a CombinedQuery
    => a b
    -> a b
    -> CombinedQuery b
union c1 c2 = CombinedQueryUnion [combinedQuery c1, combinedQuery c2]

-- | Create an UNION ALL operation between two queries.
unionAll ::
       CoerceToCombined a CombinedQuery
    => a b
    -> a b
    -> CombinedQuery b
unionAll c1 c2 = CombinedQueryUnionAll [combinedQuery c1, combinedQuery c2]

-- | Create a WHERE clause for a SELECT query.
where_ :: CoerceToCondition a (Condition b) => a -> Where b
where_ = Where . condition