{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
      AliasConstruct
    , ConditionConstruct 
    , ConstructCombinedQuery
    , JoinClauseConstruct
    , SortRefConstruct
    , (//*)
    , alias
    , asc
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
    , subQuery
    , toCombinedQuery
    , toCondition
    , toSortRef
    , toSortRefs
    , union
    , unionAll
    , w
    ) where

import Hedsql.Common.Constructor.Columns
import Hedsql.Common.Constructor.Conditions
import Hedsql.Common.Constructor.Tables
import Hedsql.Common.DataStructure.Base

import Control.Lens hiding (from)

-- private functions.

-- | Create a join on columns with a USING or ON clause.
columnJoin ::
      (TableConstructor a, TableConstructor b, JoinClauseConstruct c)
    => JoinTypeColumn -> a -> b -> c -> Join
columnJoin joinType table1 table2 clause =
    JoinColumn joinType t1 t2 jClause Nothing
    where
        t1 = table table1
        t2 = table table2
        jClause = joinClause clause
 
-- | Create a join on tables (CROSS or NATURAL join).
tableJoin ::
      (TableConstructor a, TableConstructor b)
    => JoinTypeTable -> a -> b -> Join
tableJoin joinType table1 table2 =
    JoinTable joinType (table table1) (table table2) Nothing

-- public functions.

-- | Alias construct instance for joins allowing to use an alias for a JOIN.
instance AliasConstruct Join where
    alias join name =
            setAlias join name join
        where
            setAlias (JoinTable _ _ _ _) aliasName =
                set joinTableAlias (Just aliasName)
            setAlias (JoinColumn _ _ _ _ _) aliasName =
                set joinColumnAlias (Just aliasName)

-- | Convert a value to a combined query such as an INTERSECT, EXCEPT, etc.
class ConstructCombinedQuery a where
    toCombinedQuery :: a -> CombinedQuery

instance ConstructCombinedQuery CombinedQuery where
    toCombinedQuery a = a    
 
instance ConstructCombinedQuery SelectQuery where
    toCombinedQuery = CombinedQuerySingle
   
-- | Create a JOIN clause such as ON or USING.
class JoinClauseConstruct joinClause where
    joinClause :: joinClause -> JoinClause

-- | Create an ON join clause from a predicate.
instance JoinClauseConstruct Condition where
    joinClause predicate = JoinClauseOn predicate
    
-- | Create an ON join clause from a boolean function.
instance JoinClauseConstruct FunctionBoolean where
    joinClause a = JoinClauseOn $ toCondition a

instance JoinClauseConstruct Column where
    joinClause col = JoinClauseUsing [col]

-- | Create an USING join clause from a list of columns.
instance JoinClauseConstruct [Column] where
    joinClause cols = JoinClauseUsing cols

-- | Create an USING join clause from a string which is a column name.
instance JoinClauseConstruct String where
    joinClause col = JoinClauseUsing [column col]
    
instance JoinClauseConstruct [String] where
    joinClause cols = JoinClauseUsing (columns cols)

instance TableRefConstruct Join where
    toTableRef = TableJoinReference
    toTableRefs a = [toTableRef a]

{-|
Convert a value to a sorting reference: a reference which can be used in an
ORDER BY clause.
-}
class SortRefConstruct a where
    toSortRef :: a -> SortRef
    toSortRefs :: a -> [SortRef]

instance ColRefConstruct a => SortRefConstruct a where
    toSortRef a = SortRef (toColRef a) Nothing Nothing
    toSortRefs a = [toSortRef a]

instance ColRefConstruct a => SortRefConstruct [a] where
    toSortRef = toSortRef.head
    toSortRefs = map toSortRef

instance SortRefConstruct String where
    toSortRef a = SortRef (toColRef a) Nothing Nothing
    toSortRefs a = [toSortRef a]

instance SortRefConstruct [String] where
    toSortRef = toSortRef.head
    toSortRefs = map toSortRef

instance SortRefConstruct SortRef where
    toSortRef a = a
    toSortRefs a = [a]

instance SortRefConstruct [SortRef] where
    toSortRef = head
    toSortRefs a = a

-- | Create a joker - "*" - character.
(//*) :: Joker
(//*) = Joker

{-|
Add an ascending sorting order (ASC) to a sort reference
(which can be a column reference).
-}
asc :: SortRefConstruct a => a -> SortRef
asc sortRef =  set sortRefOrder (Just Asc) (toSortRef sortRef)

-- | Create a CROSS JOIN.
crossJoin :: (TableConstructor a, TableConstructor b) => a -> b -> Join
crossJoin = tableJoin CrossJoin

-- | Apply an EXCEPT to two queries.
except ::
      (ConstructCombinedQuery a, ConstructCombinedQuery b)
    => a -> b -> CombinedQuery
except c1 c2 = CombinedQueryExcept [toCombinedQuery c1, toCombinedQuery c2]

-- | Apply an EXCEPT ALL to two queries.
exceptAll ::
      (ConstructCombinedQuery a, ConstructCombinedQuery b)
    => a -> b -> CombinedQuery
exceptAll c1 c2 =
    CombinedQueryExceptAll [toCombinedQuery c1, toCombinedQuery c2]

{-|
Add a descending sorting order (DESC) to a sort reference
(which can be a column reference).
-}
desc :: SortRefConstruct a => a -> SortRef
desc sortRef =  set sortRefOrder (Just Desc) (toSortRef sortRef)

-- | Create a FROM clause.
from :: TableRefConstruct a => a -> From
from = From . toTableRefs

-- | Create a FULL JOIN.
fullJoin ::
      (TableConstructor a, TableConstructor b, JoinClauseConstruct c)
    => a -> b -> c -> Join
fullJoin = columnJoin FullJoin

-- | Create a GROUP BY clause.
groupBy :: ColRefConstruct a => a -> GroupBy
groupBy cols = GroupBy (toColRefs cols) Nothing

-- | Add a HAVING clause to a GROUP BY clause.
having :: ConditionConstruct a => GroupBy -> a -> GroupBy
having groupByC clause =
    set groupByHaving (Just $ toCondition clause) groupByC

-- | Create a IS DISTINCT FROM operator.
isDistinctFrom ::
      (ColRefConstruct a, ColRefConstruct b)
    => a -> b -> FunctionBoolean
isDistinctFrom colRef1 colRef2 =
    IsDistinctFrom (toColRef colRef1) (toColRef colRef2)

-- | Create a IS NOT DISTINCT FROM operator.
isNotDistinctFrom ::
      (ColRefConstruct a, ColRefConstruct b)
    => a -> b -> FunctionBoolean
isNotDistinctFrom colRef1 colRef2 =
    IsNotDistinctFrom (toColRef colRef1) (toColRef colRef2)

-- | Create a INNER JOIN.
innerJoin ::
      (TableConstructor a, TableConstructor b, JoinClauseConstruct c)
    => a -> b -> c -> Join
innerJoin = columnJoin InnerJoin

-- | Apply an INTERSECT to two queries.
intersect ::
      (ConstructCombinedQuery a, ConstructCombinedQuery b)
    => a -> b -> CombinedQuery
intersect c1 c2 =
    CombinedQueryIntersect [toCombinedQuery c1, toCombinedQuery c2]

-- | Apply an INTERSECT ALL to two queries.
intersectAll ::
      (ConstructCombinedQuery a, ConstructCombinedQuery b)
    => a -> b -> CombinedQuery
intersectAll c1 c2 =
    CombinedQueryIntersectAll [toCombinedQuery c1, toCombinedQuery c2]

-- | Create a LEFT JOIN.
leftJoin ::
      (TableConstructor a, TableConstructor b, JoinClauseConstruct c)
    => a -> b -> c -> Join
leftJoin = columnJoin LeftJoin

-- | Create a LIMIT clause.
limit :: Int -> Limit
limit = Limit

-- | Create a NATURAL FULL JOIN.
naturalFullJoin :: (TableConstructor a, TableConstructor b) => a -> b -> Join
naturalFullJoin = tableJoin NaturalFullJoin

-- | Create a NATURAL LEFT JOIN.
naturalLeftJoin :: (TableConstructor a, TableConstructor b) => a -> b -> Join
naturalLeftJoin = tableJoin NaturalLeftJoin

-- | Create a NATURAL INNER JOIN.
naturalInnerJoin :: (TableConstructor a, TableConstructor b) => a -> b -> Join
naturalInnerJoin = tableJoin NaturalInnerJoin

-- | Create a NATURAL RIGHT JOIN.
naturalRightJoin :: (TableConstructor a, TableConstructor b) => a -> b -> Join
naturalRightJoin = tableJoin NaturalRightJoin

{-|
Add a nulls first option (NULLS FIRST) to a sort reference
(which can be a column reference).
-}
nullsFirst :: SortRefConstruct a => a -> SortRef
nullsFirst sortRef =  set sortRefNulls (Just NullsFirst) (toSortRef sortRef)

{-|
Add a nulls last option (NULLS LAST) to a sort reference
(which can be a column reference).
-}
nullsLast:: SortRefConstruct a => a -> SortRef
nullsLast sortRef =  set sortRefNulls (Just NullsLast) (toSortRef sortRef)

-- | Create an OFFSET clause.
offset :: Int -> Offset
offset value = Offset value

-- | Create an ORDER BY part of a query.
orderBy :: SortRefConstruct a => a -> OrderBy
orderBy cols = OrderBy (toSortRefs cols) Nothing Nothing

-- | Create a RIGHT JOIN.
rightJoin ::
      (TableConstructor a, TableConstructor b, JoinClauseConstruct c)
    => a -> b -> c -> Join
rightJoin = columnJoin RightJoin

-- | Create a SELECT query.
select :: ColRefConstruct a => a -> SelectQuery
select a =
    SelectQuery (Select (toColRefs a) n) n n n n
    where
        n = Nothing

-- | Create a SELECT DISTINCT query.
selectDistinct :: ColRefConstruct a => a -> SelectQuery     
selectDistinct a =
    SelectQuery (Select (toColRefs a) (Just Distinct)) n n n n
    where
        n = Nothing

-- | Create a SELECT DISTINCT ON query.
selectDistinctOn ::
       (ColRefConstruct a, ColRefConstruct a1)
    => [a] -- ^ Distinct references.
    ->  a  -- ^ Select clause.
    ->  SelectQuery -- ^ Select query.
selectDistinctOn distinctExpr selectExpr =
    SelectQuery (Select (toColRefs selectExpr) distinctOn) n n n n
    where
        distinctOn = Just (DistinctOn $ toExprs distinctExpr)
        n = Nothing

-- | Create a sub-query in a FROM clause.
subQuery ::
       SelectQuery    -- ^ Sub-query.
    -> String         -- ^ Alias of the sub-query.
    -> TableReference -- ^ Table reference.
subQuery sub name = SelectTableReference sub $ TableReferenceAlias name []

-- | Create an UNION operation between two queries.
union ::
      (ConstructCombinedQuery a, ConstructCombinedQuery b)
    => a -> b -> CombinedQuery
union c1 c2 = CombinedQueryUnion [toCombinedQuery c1, toCombinedQuery c2]

-- | Create an UNION ALL operation between two queries.
unionAll ::
      (ConstructCombinedQuery a, ConstructCombinedQuery b)
    => a -> b -> CombinedQuery
unionAll c1 c2 = CombinedQueryUnionAll [toCombinedQuery c1, toCombinedQuery c2]

-- | Create a WHERE clause.
w :: ConditionConstruct a => a -> Where
w = Where . toCondition