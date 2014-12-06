{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoOverloadedStrings #-}

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
import Hedsql.Common.DataStructure.Base
import Hedsql.Helpers.Coerce

import Control.Lens hiding (coerce, from)

import qualified Data.Coerce as C

import Hedsql.Common.Constructor.Functions

-- private functions.

-- | Create a join on columns with a USING or ON clause.
columnJoin ::
   (
      Coerce a [TableRef a]
   ,  Coerce b [TableRef b]
   ,  Coerce c (JoinClause c))
   => JoinTypeCol c -> a -> b -> c -> Join c
columnJoin joinType tableRef1 tableRef2 clause =
    JoinColumn
         joinType
        (C.coerce $ tableRef tableRef1)
        (C.coerce $ tableRef tableRef2)
        (joinClause clause)
 
-- | Create a join on tables (CROSS or NATURAL join).
tableJoin ::
       Coerce a [TableRef a]
    => JoinTypeTable a -> a -> a -> Join a
tableJoin joinType tableRef1 tableRef2 =
    JoinTable
        joinType
        (tableRef tableRef1)
        (tableRef tableRef2)

-- public functions.

-- | Create a combined query such as an INTERSECT, EXCEPT, etc.
combinedQuery :: Coerce a (CombinedQuery a) => a -> CombinedQuery a
combinedQuery = coerce

instance Coerce (CombinedQuery a) (CombinedQuery a) where
    coerce = id    
 
instance Coerce (Select a) (CombinedQuery a) where
    coerce = CombinedQuerySingle
   
-- | Create a JOIN clause such as ON or USING.
joinClause :: Coerce a (JoinClause a) => a -> JoinClause a
joinClause = coerce

-- | Create an ON join clause from a predicate.
instance Coerce (Condition a) (JoinClause a) where
    coerce = JoinClauseOn
    
-- | Create an ON join clause from a boolean function.
instance Coerce (FuncBool a) (JoinClause a) where
    coerce = JoinClauseOn . FuncCond

instance Coerce (Column a) (JoinClause a) where
    coerce col = JoinClauseUsing [col]

-- | Create an USING join clause from a list of columns.
instance Coerce [Column a] (JoinClause a) where
    coerce = JoinClauseUsing

-- | Create an USING join clause from a string which is a column name.
instance Coerce String (JoinClause a) where
    coerce col = JoinClauseUsing [column col]
    
instance Coerce [String] (JoinClause a) where
    coerce cols = JoinClauseUsing (columns cols)

instance Coerce (Join a) (TableRef a) where
    coerce join = TableJoinRef join Nothing

{-|
Convert a value to a sorting reference: a reference which can be used in an
ORDER BY clause.
-}
sortRef :: Coerce a [SortRef a] => a -> SortRef a
sortRef = head.coerce

sortRefs :: Coerce a [SortRef a] => a -> [SortRef a]
sortRefs = coerce

instance Coerce (ColRef a) [SortRef a] where
    coerce ref = [SortRef ref Nothing Nothing]

instance Coerce String [SortRef a] where
    coerce name = [SortRef (colRef name) Nothing Nothing]

instance Coerce (SortRef a) [SortRef a] where
    coerce ref = [ref]

-- | Create a joker - "*" - character.
(//*) :: Function a
(//*) = JokerF Joker

{-|
Add an ascending sorting order (ASC) to a sort reference
(which can be a column reference).
-}
asc :: Coerce a [SortRef a] => a -> SortRef a
asc ref =  set sortRefOrder (Just Asc) (sortRef ref)

-- | Create a CROSS JOIN.
crossJoin :: Coerce a [TableRef a] => a -> a -> Join a
crossJoin = tableJoin CrossJoin

-- | Apply an EXCEPT to two queries.
except :: Coerce a (CombinedQuery a) => a -> a -> CombinedQuery a
except c1 c2 = CombinedQueryExcept [combinedQuery c1, combinedQuery c2]

-- | Apply an EXCEPT ALL to two queries.
exceptAll :: Coerce a (CombinedQuery a) => a -> a -> CombinedQuery a
exceptAll c1 c2 =
    CombinedQueryExceptAll [combinedQuery c1, combinedQuery c2]

{-|
Add a descending sorting order (DESC) to a sort reference
(which can be a column reference).
-}
desc :: Coerce a [SortRef a] => a -> SortRef a
desc ref =  set sortRefOrder (Just Desc) (sortRef ref)

-- | Add a FROM clause to a SELECT query.
from :: Coerce a [TableRef a] => a -> From b
from = C.coerce . From . tableRefs

-- | Create a FULL JOIN.
fullJoin ::
      (Coerce a [TableRef a], Coerce b [TableRef b], Coerce c (JoinClause c))
    => a -> b -> c -> Join c
fullJoin = columnJoin FullJoin

-- | Create a GROUP BY clause.
groupBy :: Coerce a [ColRef b] => a -> GroupBy b
groupBy cols = GroupBy (colRefs cols) Nothing

-- | Add a HAVING clause to a GROUP BY clause.
having :: Coerce a (Condition b) => a -> Having b
having = Having . condition

-- | Create a IS DISTINCT FROM operator.
isDistinctFrom ::
      (Coerce a [ColRef c], Coerce b [ColRef c])
    => a -> b -> IsDistinctFrom c
isDistinctFrom colRef1 colRef2 =
    IsDistinctFrom (colRef colRef1) (colRef colRef2)

-- | Create a IS NOT DISTINCT FROM operator.
isNotDistinctFrom
  :: (Coerce a [ColRef c], Coerce b [ColRef c]) =>
     a -> b -> IsNotDistinctFrom c
isNotDistinctFrom colRef1 colRef2 =
    IsNotDistinctFrom (colRef colRef1) (colRef colRef2)

-- | Create a INNER JOIN.
innerJoin ::
      (Coerce a [TableRef a], Coerce b [TableRef b], Coerce c (JoinClause c))
    => a -> b -> c -> Join c
innerJoin = columnJoin InnerJoin

-- | Apply an INTERSECT to two queries.
intersect :: Coerce a (CombinedQuery a) => a -> a -> CombinedQuery a
intersect c1 c2 =
    CombinedQueryIntersect [combinedQuery c1, combinedQuery c2]

-- | Apply an INTERSECT ALL to two queries.
intersectAll :: Coerce a (CombinedQuery a) => a -> a -> CombinedQuery a
intersectAll c1 c2 =
    CombinedQueryIntersectAll [combinedQuery c1, combinedQuery c2]

-- | Create a LEFT JOIN.
leftJoin ::
      (Coerce a [TableRef a], Coerce b [TableRef b], Coerce c (JoinClause c))
    => a -> b -> c -> Join c
leftJoin = columnJoin LeftJoin

-- | Add a LIMIT clause to an ORDER BY part.
limit :: Int -> Limit a
limit = Limit

-- | Create a NATURAL FULL JOIN.
naturalFullJoin :: Coerce a [TableRef a] => a -> a -> Join a
naturalFullJoin = tableJoin NaturalFullJoin

-- | Create a NATURAL LEFT JOIN.
naturalLeftJoin :: Coerce a [TableRef a] => a -> a -> Join a
naturalLeftJoin = tableJoin NaturalLeftJoin

-- | Create a NATURAL INNER JOIN.
naturalInnerJoin :: Coerce a [TableRef a] => a -> a -> Join a
naturalInnerJoin = tableJoin NaturalInnerJoin

-- | Create a NATURAL RIGHT JOIN.
naturalRightJoin :: Coerce a [TableRef a] => a -> a -> Join a
naturalRightJoin = tableJoin NaturalRightJoin

{-|
Add a nulls first option (NULLS FIRST) to a sort reference
(which can be a column reference).
-}
nullsFirst :: Coerce a [SortRef a] => a -> SortRef a
nullsFirst sRef =  set sortRefNulls (Just NullsFirst) (sortRef sRef)

{-|
Add a nulls last option (NULLS LAST) to a sort reference
(which can be a column reference).
-}
nullsLast:: Coerce a [SortRef a] => a -> SortRef a
nullsLast sRef =  set sortRefNulls (Just NullsLast) (sortRef sRef)

-- | Create an OFFSET clause.
offset :: Int -> Offset a
offset = Offset

-- | Add an ORDER BY clause to a query.
orderBy :: Coerce a [SortRef a] => a -> OrderBy b
orderBy cols = C.coerce $ OrderBy (sortRefs cols) Nothing Nothing

-- | Create a RIGHT JOIN.
rightJoin ::
      (Coerce a [TableRef a], Coerce b [TableRef b], Coerce c (JoinClause c))
    => a -> b -> c -> Join c
rightJoin = columnJoin RightJoin

-- | Create a SELECT query.
select :: Coerce a [ColRef b] => a -> Select b
select a =
    Select
        (colRefs a)
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing

-- | Create a SELECT DISTINCT query.
selectDistinct :: Coerce a [ColRef b] => a -> Select b   
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
       (Coerce a [ColRef a], Coerce b [ColRef a])
    => [a]       -- ^ Distinct references.
    ->  b        -- ^ Select clause.
    ->  Select c -- ^ Select query.
selectDistinctOn distinctExpr selectExpr =
    s
    where
        s :: Select a
        s = C.coerce $ 
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
union :: Coerce a (CombinedQuery a) => a -> a -> CombinedQuery a
union c1 c2 = CombinedQueryUnion [combinedQuery c1, combinedQuery c2]

-- | Create an UNION ALL operation between two queries.
unionAll :: Coerce a (CombinedQuery a) => a -> a -> CombinedQuery a
unionAll c1 c2 = CombinedQueryUnionAll [combinedQuery c1, combinedQuery c2]

-- | Create a WHERE clause for a SELECT query.
where_ :: Coerce a (Condition b) => a -> Where b
where_ = Where . condition