{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

{-|
Module      : Hedsql/Commun/DataStructure/Select.hs
Description : Data structure of the SELECT statements and key components.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Data structure of the SELECT statement and of the key components
such as columns and tables.
-}
module Hedsql.Common.DataStructure.Select where

import Control.Lens

-- Types

type Label = String

-- Values.

-- | SQL data types.
data SqlDataType =
     Date
   | Char Int
   | Varchar Int
   | SqlChar Int
   -- Numeric types
   | SmallInt -- ^ 2 bytes integer - range from -32768 to +32767.
   | Integer -- ^ 4 bytes integer - range from -2147483648 to +2147483647.
   | BigInt -- ^ 8 bytes integer -
            --   range from -9223372036854775808 to +9223372036854775807.
     deriving (Show)

-- | SQL values.
data SqlValue =
      SqlValueDefault
    | SqlValueInt Int
    | SqlValueNull
    | SqlValueString String
      deriving (Show)

-- Table components.

-- | Table definition.
data Table a = Table {
      _tableName  :: String
    , _tableAlias :: Maybe (TableRefAs a)
    } deriving (Show)

{-|
    A table reference might be a table, a table join or a sub-query used
    in a FROM clause.
-}
data TableRef a =
      LateralTableRef (Select a) (TableRefAs a)
    | SelectTableRef  (Select a) (TableRefAs a)
    | TableJoinRef    (Join a)
    | TableTableRef   (Table a)
    deriving (Show)

-- | Table reference (table or join) alias. 
data TableRefAs a = TableRefAs
    {
      _tableRefAliasName    ::  String
    , _tableRefAliasColumns :: [String]
    } deriving (Show)

-- Column components

-- | Column definition used for data queries such as SELECT queries.
data Column a = Column
    {
      _colName        :: String
    , _colDataType    :: Maybe SqlDataType
    , _colConstraints :: Maybe [ColConstraint a]
    , _colTable       :: Maybe (Table a) -- ^ If provided, the qualified column
                                         --   name will be used.
    } deriving (Show)

-- | Constraint on a column.
data ColConstraint a = ColConstraint
    {
      _colConstraintName :: Maybe String
    , _colConstraintType :: (ColConstraintType a)
    } deriving (Show)

-- | Column constraints types.
data ColConstraintType a =
      Check (Condition a)
    -- TODO: | Collate
    | Default (Expression a)
    | NotNull
    | Null
    | Primary Bool  -- ^ Primary key.
                    --   If true, the primary key will have an AUTOINCREMENT.
    | Reference (Table a) (Column a) (Maybe OnAction) -- ^ Foreign key.
    | Unique
      deriving (Show)

{- |
Generic definition of a column reference used in the SELECT clause of the query.
-}
data ColRef a = ColRef
    {
      _colRefExpr :: Expression a
    , _colRefLabel :: Maybe Label
    } deriving (Show)

-- | Actions to be performed on foreign keys.
data OnAction =
      OnDelete SqlAction
    | OnUpdate SqlAction
      deriving (Show)

-- | Action to perform when an entry in a table is updated or deleted.
data SqlAction =
      Cascade
    | NoAction
    | Restrict
    | SetDefault
    | SetNull
      deriving (Show) 

-- Expression.

{-|
Generic expression which can then be used in a SELECT or WHERE part.
An expression can either be:
- the name of a column
- a function
- a select query
- a value.
-}
data Expression a =
      ColExpr (Column a)
    | FuncExpr (Function a)
    | SelectExpr (Select a)
    | ValueExpr SqlValue
    | ValueExprs [SqlValue]
      deriving (Show)

-- SELECT components.

-- | SELECT query.
data Select a = Select
    {
      _selectColRef  :: [ColRef a]
    , _selectType    :: Maybe (SelectType a)
    , _fromClause    :: Maybe (From a)
    , _whereClause   :: Maybe (Where a)
    , _groupByClause :: Maybe (GroupBy a)
    , _orderByClause :: Maybe (OrderBy a)
} deriving (Show)

-- | Type of SELECT query (ALL or DISTINCT).
data SelectType a =
      All
   |  Distinct
   |  DistinctOn [Expression a]
      deriving (Show)


-- | FROM part of a query.
data From a =
    From [TableRef a]
    deriving (Show)

{-|
A JOIN between two tables.

The JoinTable are joins which take only tables as parameter - CROSS and NATURAL
joins -.
The JoinColumn are the joins having a ON or USING clause.

An alias can be defined in the Maybe String type.
-}
data Join a =
    JoinTable
    {
      _joinTableType   :: JoinTypeTable a
    , _joinTableTable1 :: (Table a)
    , _joinTableTable2 :: (Table a)
    , _joinTableAlias  :: Maybe String
    }
    | JoinColumn
    {
      _joinColumnType   :: JoinTypeCol a
    , _joinColumnTable1 :: (Table a)
    , _joinColumnTable2 :: (Table a)
    , _joinColumnClause :: (JoinClause a)
    , _joinColumnAlias  :: Maybe String
    } deriving (Show)

-- | JOIN clause: ON or USING.
data JoinClause a =
      JoinClauseOn (Condition a)
    | JoinClauseUsing [Column a]
      deriving (Show)

-- | JOIN on columns.
data JoinTypeCol a =
      FullJoin 
    | LeftJoin
    | InnerJoin
    | RightJoin
    deriving (Show)

-- | JOIN on tables.
data JoinTypeTable a =
      CrossJoin
    | NaturalInnerJoin
    | NaturalLeftJoin
    | NaturalRightJoin
    | NaturalFullJoin
    deriving (Show)

{- |
WHERE part of the query consisting of a condition. A can be single predicate or
an AND or OR list of conditions.
-}
data Where a = Where (Condition a)
    deriving (Show)

-- | SQL Condition.
data Condition a =
      FuncCond (FuncBool a)
    | And [Condition a]
    | Or [Condition a]
      deriving (Show)

-- | GROUP BY query part.
data GroupBy a = GroupBy {
      _groupByCols   :: [ColRef a]
    , _groupByHaving :: Maybe (Condition a)
   } deriving (Show)

{- |
ORDER BY query part.

LIMIT and OFFSET can also be specified on top of the ORDER BY.
Those are specified here - and not as query parts - because defining a limit or
an offset without any order by clause would result in inconsistant results since
SQL does not guarantee any specific return order unless explicitely specified.
-}
data OrderBy a = OrderBy {
      _partOrderByColumns :: [SortRef a]
    , _partOrderByLimit   :: Maybe Int
    , _partOrderByOffset  :: Maybe Int
} deriving (Show)

-- | NULLS FIRST and NULLS LAST parameters for sorting.
data SortNulls a =
      NullsFirst
    | NullsLast
      deriving (Show)

-- | Sorting order.
data SortOrder a =
      Asc
    | Desc
      deriving (Show)

-- | Defines how a given column has to be sorted.
data SortRef a = SortRef {
      _sortRefCol   :: ColRef a
    , _sortRefOrder :: Maybe (SortOrder a)
    , _sortRefNulls :: Maybe (SortNulls a)
} deriving (Show)

-- | Combined query such as UNION.
data CombinedQuery a =
      CombinedQuerySingle (Select a)
    | CombinedQueryExcept [CombinedQuery a]
    | CombinedQueryExceptAll [CombinedQuery a]
    | CombinedQueryIntersect [CombinedQuery a]
    | CombinedQueryIntersectAll [CombinedQuery a]
    | CombinedQueryUnion [CombinedQuery a]
    | CombinedQueryUnionAll [CombinedQuery a]
      deriving (Show)

-- Functions.

data Function a =
    -- Operators.
      AddF (Add a)                     -- ^ Addition ("+") operator.
    | BitAndF (BitAnd a)               -- ^ Bitwise AND ("&") operator.
    | BitOrF (BitOr a)                 -- ^ Bitwise OR ("|") operator.
    | BitShiftLeftF (BitShiftLeft a)   -- ^ Bitwise shift left  ("<<")
                                       --   operator.
    | BitShiftRightF (BitShiftRight a) -- ^ Bitwise shift right  (">>")
                                       --   operator.
                                       --   operator.
    | DivideF (Divide a)               -- ^ Division ("/") operator.
    | ModuloF (Modulo a)               -- ^ Modulo - remainer - ("%")
                                       --   operator.
    | MultiplyF (Multiply a)           -- ^ "*" Multiplication operator.
    | SubstractF (Substract a)         -- ^ Subtraction "-" operator.

    -- Functions.
    | CountF       (Count a)
    | CurrentDateF (CurrentDate a)
    | MaxF         (Max a)
    | MinF         (Min a)
    | JokerF       (Joker a) -- ^ TODO: have it somewhere else,
                             --         since it is a colref!
    | RandomF      (Random a)
    | SumF         (Sum a)
      deriving (Show)

data Add a = Add (ColRef a) (ColRef a) deriving (Show)
data BitAnd a= BitAnd (ColRef a) (ColRef a) deriving (Show)
data BitOr a = BitOr (ColRef a) (ColRef a) deriving (Show)
data BitShiftLeft a = BitShiftLeft (ColRef a) (ColRef a) deriving (Show)
data BitShiftRight a = BitShiftRight (ColRef a) (ColRef a) deriving (Show)
data Divide a = Divide (ColRef a) (ColRef a) deriving (Show) 
data Modulo a = Modulo (ColRef a) (ColRef a) deriving (Show)
data Multiply a = Multiply (ColRef a) (ColRef a) deriving (Show)
data Substract a = Substract (ColRef a) (ColRef a) deriving (Show)

data Count a = Count (Expression a) deriving (Show)
data CurrentDate a = CurrentDate a deriving (Show)
data Max a = Max (Expression a) deriving (Show)
data Min a = Min (Expression a) deriving (Show)
data Joker a= Joker deriving (Show) 
data Random a = Random deriving (Show)
data Sum a = Sum (Expression a) deriving (Show)

-- | Functions returning TRUE or FALSE.
data FuncBool a =
      BetweenF (Between a)                     -- ^ BETWEEN
    | ExistsF (Exists a)                       -- ^ EXISTS
    | IsFalseF (IsFalse a)                     -- ^ IS FALSE
    | IsNotFalseF (IsNotFalse a)               -- ^ IS NOT FALSE
    | IsNotNullF (IsNotNull a)                 -- ^ IS NOT NULL
    | IsNotTrueF (IsNotTrue a)                 -- ^ IS NOT TRUE
    | IsNotUnknownF (IsNotUnknown a)           -- ^ IS NOT UNKNOWN
    | IsNullF (IsNull a)                       -- ^ IS NULL
    | IsTrueF (IsTrue a)                       -- ^ IS TRUE
    | IsUnknownF (IsUnknown a)                 -- ^ IS UNKNOWN
    | NotBetweenF (NotBetween a)               -- ^ NOT BETWEEN
    | EqualF (Equal a)                         -- ^ =
    | GreaterThanF (GreaterThan a)             -- ^ \>
    | GreaterThanOrEqToF (GreaterThanOrEqTo a) -- ^ \>=
    | InF (In a)                               -- ^ IN
    | IsDistinctFromF (IsDistinctFrom a)       -- ^ IS DISTINCT FROM
    | IsNotDistinctFromF (IsNotDistinctFrom a) -- ^ IS NOT DISTINCT FROM
    | LikeF (Like a)                           -- ^ LIKE
    | NotEqualF (NotEqual a)                   -- ^ <>
    | NotInF (NotIn a)                         -- ^ NOT IN
    | SmallerThanF (SmallerThan a)             -- ^ <
    | SmallerThanOrEqToF (SmallerThanOrEqTo a) -- ^ \<=
    deriving (Show)

data Between a = Between (ColRef a) (ColRef a) (ColRef a) deriving (Show)
data Exists a = Exists (ColRef a) deriving (Show)
data IsFalse a = IsFalse (ColRef a) deriving (Show)
data IsNotFalse a = IsNotFalse (ColRef a) deriving (Show)
data IsNotNull a = IsNotNull (ColRef a) deriving (Show)
data IsNotTrue a = IsNotTrue (ColRef a) deriving (Show)
data IsUnknown a = IsUnknown (ColRef a) deriving (Show)
data IsNotUnknown a = IsNotUnknown (ColRef a) deriving (Show)
data NotBetween a = NotBetween (ColRef a) (ColRef a) (ColRef a) deriving (Show)
data Equal a = Equal (ColRef a) (ColRef a) deriving (Show)
data GreaterThan a = GreaterThan (ColRef a) (ColRef a) deriving (Show)
data GreaterThanOrEqTo a =
    GreaterThanOrEqTo (ColRef a) (ColRef a) deriving (Show)
data In a = In (ColRef a) (ColRef a) deriving (Show)
data IsDistinctFrom a = IsDistinctFrom (ColRef a) (ColRef a)  deriving (Show)
data IsNotDistinctFrom a =
    IsNotDistinctFrom (ColRef a) (ColRef a) deriving (Show)
data IsNull a = IsNull (ColRef a) deriving (Show)
data IsTrue a = IsTrue (ColRef a) deriving (Show)
data Like a = Like (ColRef a) (ColRef a) deriving (Show)
data NotEqual a = NotEqual (ColRef a) (ColRef a) deriving (Show)
data NotIn a = NotIn (ColRef a) (ColRef a) deriving (Show)
data SmallerThan a = SmallerThan (ColRef a) (ColRef a) deriving (Show)
data SmallerThanOrEqTo a =
    SmallerThanOrEqTo (ColRef a) (ColRef a) deriving (Show)

-- Lenses.
makeLenses ''GroupBy
makeLenses ''Join
makeLenses ''From
makeLenses ''OrderBy
makeLenses ''Where
makeLenses ''ColConstraint
makeLenses ''ColRef
makeLenses ''Column
makeLenses ''Select
makeLenses ''SortRef
makeLenses ''Table
makeLenses ''TableRefAs