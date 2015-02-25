{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Database/Hedsql/Commun/DataStructure/Select.hs
Description : Data structure of the SELECT statements and key components.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Data structure of the SELECT statement and of the key components
such as columns and tables.
-}
module Database.Hedsql.Common.DataStructure.Select where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Control.Lens

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- Values
----------------------------------------

-- | SQL data types.
data SqlDataType a =
     Boolean
   | Date
   -- Character types
   | Char Int
   | Varchar Int
   -- Numeric types
   | SmallInt -- ^ 2 bytes integer - range from -32768 to +32767.
   | Integer  -- ^ 4 bytes integer - range from -2147483648 to +2147483647.
   | BigInt   -- ^ 8 bytes integer -
              --   range from -9223372036854775808 to +9223372036854775807. 
     deriving (Show)

-- | SQL values.
data SqlValue a =
      SqlValueBool Bool
    | SqlValueDefault
    | SqlValueInt Int
    | SqlValueNull
    | SqlValueString String
    | Placeholder
      deriving (Show)

----------------------------------------
-- View
----------------------------------------

-- | CREATE VIEW query.
data CreateView a = CreateView
    { _viewName   :: String
    , _viewSelect :: Select a
    } deriving (Show)

----------------------------------------
-- Tables
----------------------------------------

{-|
A table reference can be a real table, or a derived table such as
a table join or a sub-query.
Table references are usually used in FROM clauses or joins.
-}
data TableRef a =
      LateralTableRef (Select a)        (TableRefAs a)
    | SelectTableRef  (Select a)        (TableRefAs a)
    | TableJoinRef    (Join a)   (Maybe (TableRefAs a))
    | TableTableRef   (Table a)  (Maybe (TableRefAs a))
      deriving (Show)

-- | Table reference (table or join) alias. 
data TableRefAs a = TableRefAs
    { _tableRefAliasName    ::  String
    , _tableRefAliasColumns :: [String]
    } deriving (Show)

-- | Table definition.
data Table a = Table
    { _tableIfNotExists :: Bool -- ^ If true the IF NOT EXISTS clause is added
    , _tableName        :: String
    , _tableCols        :: [Column a]
    , _tableConstraints :: Maybe [TableConstraint a]
    } deriving (Show)

-- | Table constraints to be used in CREATE statement.
data TableConstraint a = TableConstraint
    { _tableConstraintName       :: Maybe String
    , _tableConstraintConstraint :: TableConstraintType a
    , _tableConstraintTiming     :: Maybe (ConstraintTiming a)
    } deriving (Show)

-- | Table constraints types used in CREATE statement.
data TableConstraintType a =
      ForeignKey [Column a] (ForeignKeyClause a)
    | TableConstraintPrimaryKey [Column a]
    | TableConstraintUnique [Column a]
    | TableConstraintCheck (Condition a)
      deriving (Show)

-- | Foreign key clause to be used in a table constraint of a CREATE statement.
data ForeignKeyClause a = ForeignKeyClause
    { _foreignKeyClauseTable  :: Table a
    , _foreignKeyClauseCols   :: [Column a]
    , _foreignKeyMatch        :: Maybe (Match a)
    , _foreignKeyClauseAction :: Maybe (OnAction a)
    } deriving (Show)

-- | Foreign key match type.
data Match a =
      Full
    | Partial
    | Simple
      deriving (Show)
      
-- | Timing of a constraint.
data ConstraintTiming a = ConstraintTiming
    { _constraintTimingType  :: ConstraintTimingType a
    , _constraintTimingCheck :: ConstraintTimingCheck a
    } deriving (Show)

-- | Type of a timing constraint.
data ConstraintTimingType a =
      Deferable
    | NotDeferable
      deriving (Show)

-- | Timing of a timing constraint.
data ConstraintTimingCheck a =
      InitiallyImmediate
    | InitiallyDeferred
      deriving (Show)
      
-- | Column definition used for data queries such as SELECT queries.
data Column a = Column
    { _colName        :: String
    , _colDataType    :: Maybe (SqlDataType a)
    , _colConstraints :: Maybe [ColConstraint a]
    } deriving (Show)

-- | Constraint on a column.
data ColConstraint a = ColConstraint
    { _colConstraintName :: Maybe String
    , _colConstraintType :: ColConstraintType a
    } deriving (Show)

-- | Column constraints types.
data ColConstraintType a =
      Check     (Condition a)
    -- TODO: | Collate
    | Default   (Expression a)
    | NotNull
    | Null
    | Primary   Bool  -- ^ Primary key.
                      --   If true, the primary key will have an AUTOINCREMENT.
    | Reference (Table a) (Column a) (Maybe (OnAction a)) -- ^ Foreign key.
    | Unique
      deriving (Show)

{-|
Generic definition of a column reference used in the SELECT clause of the query.
-}
data ColRef a = ColRef
    { _colRefExpr  :: Expression a
    , _colRefLabel :: Maybe String
    } deriving (Show)

-- | Actions to be performed on foreign keys.
data OnAction a =
      OnDelete (SqlAction a)
    | OnUpdate (SqlAction a)
      deriving (Show)

-- | Action to perform when an entry in a table is updated or deleted.
data SqlAction a =
      Cascade
    | NoAction
    | Restrict
    | SetDefault
    | SetNull
      deriving (Show) 

----------------------------------------
-- Expression
----------------------------------------

{-|
Generic expression which can then be used in a SELECT or WHERE part.
An expression can either be:
- the name of a column
- a function
- a select query
- a value.
-}
data Expression a =
      ColExpr
          { _colExpr           :: (Column a)
          , _colExprTableLabel :: Maybe (TableRef a) -- ^ Qualified column name.
          }
    | CondExpr   (Condition a)
    | FuncExpr   (Function a)
    | SelectExpr (Select a)
    | ValueExpr  (SqlValue a)
    | ValueExprs [SqlValue a]
      deriving (Show)

----------------------------------------
-- SELECT
----------------------------------------

-- | SELECT query.
data Select a = Select
    { _selectColRef  :: [ColRef a]
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

The JoinTable are joins which take only tables references as parameter
- CROSS and NATURAL joins -.
The JoinColumn are the joins having a ON or USING clause.
-}
data Join a =
    JoinTable
    { _joinTableType   :: JoinTypeTable a
    , _joinTableTable1 :: TableRef a
    , _joinTableTable2 :: TableRef a
    }
    | JoinColumn
    { _joinColumnType   :: JoinTypeCol a
    , _joinColumnTable1 :: TableRef a
    , _joinColumnTable2 :: TableRef a
    , _joinColumnClause :: JoinClause a
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

-- | LIMIT clause.
data Limit a = Limit
    { _limitVal :: Int
    } deriving (Show)

{- |
WHERE part of the query consisting of a condition. A can be single predicate or
an AND or OR list of conditions.
-}
data Where a =
    Where (Condition a)
    deriving (Show)

-- | SQL Condition.
data Condition a =
      FuncCond (FuncBool  a)
    | And      [Condition a]
    | Or       [Condition a]
      deriving (Show)

-- | GROUP BY clause.
data GroupBy a = GroupBy
    { _groupByCols   :: [ColRef a]
    , _groupByHaving :: Maybe (Having a)
    } deriving (Show)

-- | HAVING clause.
data Having a =
    Having (Condition a)
    deriving (Show)

-- | OFFSET clause.
data Offset a = Offset
    { offsetVal :: Int
    } deriving (Show)

{- |
ORDER BY query part.

LIMIT and OFFSET can also be specified on top of the ORDER BY.
Those are specified here - and not as query parts - because defining a limit or
an offset without any order by clause would result in inconsistant results since
SQL does not guarantee any specific return order unless explicitely specified.
-}
data OrderBy a = OrderBy
    { _partOrderByColumns :: [SortRef a]
    , _partOrderByLimit   :: Maybe (Limit a)
    , _partOrderByOffset  :: Maybe (Offset a)
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
data SortRef a = SortRef
    { _sortRefCol   :: ColRef a
    , _sortRefOrder :: Maybe (SortOrder a)
    , _sortRefNulls :: Maybe (SortNulls a)
    } deriving (Show)

-- | Combined query such as UNION.
data CombinedQuery a =
      Single       (Select a)
    | Except       [CombinedQuery a]
    | ExceptAll    [CombinedQuery a]
    | Intersect    [CombinedQuery a]
    | IntersectAll [CombinedQuery a]
    | Union        [CombinedQuery a]
    | UnionAll     [CombinedQuery a]
      deriving (Show)

----------------------------------------
-- Functions
----------------------------------------

data Function a =
    -- Operators.
      Add           (ColRef a) (ColRef a) -- ^ Addition ("+") operator.
    | BitAnd        (ColRef a) (ColRef a) -- ^ Bitwise AND ("&") operator.
    | BitOr         (ColRef a) (ColRef a) -- ^ Bitwise OR ("|") operator.
    | BitShiftLeft  (ColRef a) (ColRef a) -- ^ Bitwise shift left  ("<<")
                                          --   operator.
    | BitShiftRight (ColRef a) (ColRef a) -- ^ Bitwise shift right  (">>")
                                          --   operator.
    | Divide        (ColRef a) (ColRef a) -- ^ Division ("/") operator.
    | Modulo        (ColRef a) (ColRef a) -- ^ Modulo - remainer - ("%")
                                          --   operator.
    | Multiply      (ColRef a) (ColRef a) -- ^ "*" Multiplication operator.
    | Substract     (ColRef a) (ColRef a) -- ^ Subtraction "-" operator.

    -- Functions.
    
    -- Basic functions.
    
    | Count       (Expression a)
    | CurrentDate
    | Max         (Expression a)
    | Min         (Expression a)
    | Joker                       -- ^ TODO: have it somewhere else,
                                  -- since it is a colref!
    | Random
    | Sum         (Expression a)
    
    -- MariaDB specific functions.
    
    | CalcFoundRows
    | FoundRows
      deriving (Show)

{-|
Functions returning TRUE or FALSE.

Note: boolean values themselves are not condisered as part of those functions.
They are considered as values.
Therefore, it is not possible to write statements such as "TRUE IS TRUE".
-}
data FuncBool a =
      Between           (ColRef a) (ColRef a) (ColRef a) -- ^ BETWEEN
    | Exists            (ColRef a)                       -- ^ EXISTS
    | IsFalse           (ColRef a)                       -- ^ IS FALSE
    | IsNotFalse        (ColRef a)                       -- ^ IS NOT FALSE
    | IsNotNull         (ColRef a)                       -- ^ IS NOT NULL
    | IsNotTrue         (ColRef a)                       -- ^ IS NOT TRUE
    | IsNotUnknown      (ColRef a)                       -- ^ IS NOT UNKNOWN
    | IsNull            (ColRef a)                       -- ^ IS NULL
    | IsTrue            (ColRef a)                       -- ^ IS TRUE
    | IsUnknown         (ColRef a)                       -- ^ IS UNKNOWN
    | NotBetween        (ColRef a) (ColRef a) (ColRef a) -- ^ NOT BETWEEN
    | Equal             (ColRef a) (ColRef a)            -- ^ =
    | GreaterThan       (ColRef a) (ColRef a)            -- ^ \>
    | GreaterThanOrEqTo (ColRef a) (ColRef a)            -- ^ \>=
    | In                (ColRef a) (ColRef a)            -- ^ IN
    | IsDistinctFrom    (ColRef a) (ColRef a)            -- ^ IS DISTINCT FROM
    | IsNotDistinctFrom (ColRef a) (ColRef a)            -- ^ IS NOT DISTINCT
                                                         --   FROM
    | Like              (ColRef a) (ColRef a)            -- ^ LIKE
    | NotEqual          (ColRef a) (ColRef a)            -- ^ <>
    | NotIn             (ColRef a) (ColRef a)            -- ^ NOT IN
    | SmallerThan       (ColRef a) (ColRef a)            -- ^ <
    | SmallerThanOrEqTo (ColRef a) (ColRef a)            -- ^ \<=
    deriving (Show)

----------------------------------------
-- Lenses
----------------------------------------

makeLenses ''GroupBy
makeLenses ''Join
makeLenses ''From
makeLenses ''OrderBy
makeLenses ''Where
makeLenses ''ColConstraint
makeLenses ''ColRef
makeLenses ''Column
makeLenses ''Expression
makeLenses ''Limit
makeLenses ''Offset
makeLenses ''Select
makeLenses ''SortRef
makeLenses ''Table
makeLenses ''TableConstraint
makeLenses ''TableRefAs
makeLenses ''ConstraintTiming
makeLenses ''CreateView
makeLenses ''ForeignKeyClause