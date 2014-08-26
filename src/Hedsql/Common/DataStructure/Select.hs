-- file : Hedsql/Commun/DataStructure/Select
{-# LANGUAGE
      ExistentialQuantification
    , FlexibleContexts
    , FlexibleInstances
    , MultiParamTypeClasses
    , TemplateHaskell
    , UndecidableInstances #-}

{-|
    Data structure of the SELECT statement and of the key components
    such as columns and tables.
-}

module Hedsql.Common.DataStructure.Select where

import Hedsql.Common.Driver
import Hedsql.Common.Parser
import Control.Lens

-- Types

type Label = String

-- Values.

-- | SQL values.
data SqlValue =
      SqlValueDefault
    | SqlValueInt Int
    | SqlValueNull
    | SqlValueString String
      deriving (Show)

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

-- Table components.

-- | Table definition.
data Table = Table {
      _tableName  :: String
    , _tableAlias :: Maybe TableReferenceAlias
    } deriving (Show)

{-|
    A table reference might be a table, a table join or a sub-query used
    in a FROM clause.
-}
data TableReference =
      TableTableReference Table
    | TableJoinReference Join
    | LateralTableReference SelectQuery TableReferenceAlias
    | SelectTableReference SelectQuery TableReferenceAlias
    deriving (Show)

-- | Table reference (table or join) alias. 
data TableReferenceAlias = TableReferenceAlias {
      _tableReferenceAliasName :: String
    , _tableReferenceAliasColumns :: [String]
} deriving (Show)

-- Column components

-- | Column definition used for data queries such as SELECT queries.
data Column = Column {
      _colName :: String
    , _colDataType :: Maybe SqlDataType
    , _colConstraints :: Maybe [ColConstraint]
    , _colTable :: Maybe Table -- ^ If provided, the qualified column name will
                               --   be used.
} deriving (Show)

-- | Constraint on a column.
data ColConstraint = ColConstraint {
        _colConstraintName :: Maybe String
      , _colConstraintType :: ColConstraintType
} deriving (Show)

-- | Column constraints types.
data ColConstraintType =
      Check Condition
    -- TODO: | Collate
    | Default Expression
    | NotNull
    | Null
    | Primary Bool -- ^ Primary key.
                   --   If true, the primary key will have an AUTOINCREMENT.
    | Reference Table Column (Maybe OnAction) -- ^ Foreign key.
    | Unique
      deriving (Show)

{- |
 Generic definition of a column reference used in the SELECT clause
 of the query.
-}
data ColRef = ColRef {
      _colRefExpr :: Expression
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
data Expression =
      ColExpr Column
    | FuncExpr Function
    | OperatorExpr Operator
    | SelectExpr SelectQuery
    | ValueExpr SqlValue
    | ValueExprs [SqlValue]
      deriving (Show)

-- SELECT components.

-- | SELECT query.
data SelectQuery = SelectQuery {
      _selectClause :: Select
    , _fromClause   :: Maybe From
    , _whereClause :: Maybe Where
    , _groupByClause :: Maybe GroupBy
    , _orderByClause :: Maybe OrderBy
} deriving (Show)

-- | SELECT part of a query.
data Select = Select {
      _selectColRef :: [ColRef]
    , _selectType :: Maybe SelectType
} deriving (Show)

-- | Type of SELECT query (ALL or DISTINCT).
data SelectType =
      All
   |  Distinct
   |  DistinctOn [Expression]
      deriving (Show)


-- | FROM part of a query.
data From =
    From [TableReference]
    deriving (Show)

{-|
    A JOIN between two tables.
    
    The JoinTable are joins which take only tables as parameter - CROSS and NATURAL joins -.
    The JoinColumn are the joins having a ON or USING clause.
    
    An alias can be defined in the Maybe String type.
-}
data Join =
      JoinTable {
      _joinTableType :: JoinTypeTable
    , _joinTableTable1 :: Table
    , _joinTableTable2 :: Table
    , _joinTableAlias :: Maybe String
    }
    | JoinColumn {
      _joinColumnType :: JoinTypeColumn
    , _joinColumnTable1 :: Table
    , _joinColumnTable2 :: Table
    , _joinColumnClause :: JoinClause
    , _joinColumnAlias :: Maybe String
    }
    deriving (Show)

-- | JOIN clause: ON or USING.
data JoinClause =
      JoinClauseOn Condition
    | JoinClauseUsing [Column]
      deriving (Show)

-- | JOIN on columns.
data JoinTypeColumn =
      FullJoin 
    | LeftJoin
    | InnerJoin
    | RightJoin
    deriving (Show)

-- | JOIN on tables.
data JoinTypeTable =
      CrossJoin
    | NaturalInnerJoin
    | NaturalLeftJoin
    | NaturalRightJoin
    | NaturalFullJoin
    deriving (Show)

{- |
    WHERE part of the query consisting of a condition. A can be single predicate or an AND or OR
    list of conditions.
-}
data Where = Where Condition
    deriving (Show)

-- | SQL Condition.
data Condition =
      FunctionCondition FunctionBoolean
    | And [Condition]
    | Or [Condition]
      deriving (Show)

-- | GROUP BY query part.
data GroupBy = GroupBy {
      _groupByCols :: [ColRef]
    , _groupByHaving :: Maybe Condition
   } deriving (Show)

{- |
    ORDER BY query part.
    
    LIMIT and OFFSET can also be specified on top of the ORDER BY. Those are specified here
    - and not as query parts - because defining a limit or an offset without any order by clause
    would result in inconsistant results since SQL does not guarantee any specific return order
    unless explicitely specified.
-}
data OrderBy = OrderBy {
      _partOrderByColumns :: [SortRef]
    , _partOrderByLimit :: Maybe Limit
    , _partOrderByOffset :: Maybe Offset
} deriving (Show)

-- | NULLS FIRST and NULLS LAST parameters for sorting.
data SortNulls =
      NullsFirst
    | NullsLast
      deriving (Show)

-- | Sorting order.
data SortOrder =
      Asc
    | Desc
      deriving (Show)

-- | Defines how a given column has to be sorted.
data SortRef = SortRef {
      _sortRefCol :: ColRef
    , _sortRefOrder :: Maybe SortOrder
    , _sortRefNulls :: Maybe SortNulls
} deriving (Show)

-- | OFFSET clause
data Offset = Offset {
    _offsetValue :: Int
} deriving (Show)

-- | LIMIT clause
data Limit = Limit {
    _limitValue :: Int
} deriving (Show)

-- | Combined query such as UNION.
data CombinedQuery =
      CombinedQuerySingle SelectQuery
    | CombinedQueryExcept [CombinedQuery]
    | CombinedQueryExceptAll [CombinedQuery]
    | CombinedQueryIntersect [CombinedQuery]
    | CombinedQueryIntersectAll [CombinedQuery]
    | CombinedQueryUnion [CombinedQuery]
    | CombinedQueryUnionAll [CombinedQuery]
      deriving (Show)

-- Functions.

data Function = forall a. (Functionable a, Show a) => Function a

-- Define the functions.
class Functionable b where
    toSqlFunctionString :: (
          Driver a
        , Parser a CurrentDate
        , Parser a Joker
        , Parser a Random
        , Parser a Sum
        ) => a -> b -> String

instance Show Function where
     show (Function a) = show a

instance Functionable Joker where
    toSqlFunctionString = toSqlString

instance Functionable CurrentDate where
    toSqlFunctionString = toSqlString

instance Functionable Random where
    toSqlFunctionString = toSqlString

instance Functionable Sum where
    toSqlFunctionString = toSqlString

-- | COUNT function.
data Count = Count Expression deriving (Show)

-- | Generic current date function which shall be customised depending on the vendor.
data CurrentDate = CurrentDate deriving (Show)

-- | MAX function.
data Max = Max Expression deriving (Show)

-- | MIN function.
data Min = Min Expression deriving (Show)

-- | SUM function.
data Sum = Sum Expression deriving (Show)

-- | RANDOM function.
data Random = Random deriving (Show)

-- | * character.
data Joker = Joker deriving (Show)

-- | Functions returning TRUE or FALSE.
data FunctionBoolean =
      Between ColRef ColRef ColRef
    | Exists ColRef
    | IsFalse ColRef
    | IsNotFalse ColRef
    | IsNotNull ColRef
    | IsNotTrue ColRef
    | IsNotUnknown ColRef
    | IsNull ColRef
    | IsTrue ColRef
    | IsUnknown ColRef
    | NotBetween ColRef ColRef ColRef
    | Equal ColRef ColRef -- ^ =
    | GreaterThan ColRef ColRef -- ^ >
    | GreaterThanOrEqualTo ColRef ColRef -- ^ >=
    | In ColRef ColRef -- ^ IN
    | IsDistinctFrom ColRef ColRef -- ^ IS DISTINCT FROM
    | IsNotDistinctFrom ColRef ColRef -- ^ IS NOT DISTINCT FROM
    | Like ColRef ColRef -- ^ LIKE
    | NotEqual ColRef ColRef -- ^ <>
    | NotIn ColRef ColRef -- ^ NOT IN
    | SmallerThan ColRef ColRef -- ^ <
    | SmallerThanOrEqualTo ColRef ColRef -- ^ <=
    deriving (Show)

-- | Operators such as "+", "-" or "*".
data Operator =
      Abs ColRef ColRef -- ^ Absolute value ("@") operator.
    | Add ColRef ColRef -- ^ Addition ("+") operator.
    | BitwiseAnd ColRef ColRef -- ^ Bitwise AND ("&") operator.
    | BitwiseOr ColRef ColRef -- ^ Bitwise OR ("|") operator.
    | BitwiseShiftLeft ColRef ColRef -- ^ Bitwise shift left  ("<<") operator.
    | BitwiseShiftRight ColRef ColRef -- ^ Bitwise shift right  (">>") operator.
    | Concatenate ColRef ColRef -- ^ String concatenation ("||") operator.
    | Divide ColRef ColRef -- ^ Division ("/") operator.
    | Modulo ColRef ColRef -- ^ Modulo - remainder - ("%") operator.
    | Multiply ColRef ColRef -- ^ "*" Multiplication operator.
    | Substract ColRef ColRef -- ^ Substraction "-" operator.
      deriving (Show)

-- Lenses.
makeLenses ''GroupBy
makeLenses ''Join
makeLenses ''Limit
makeLenses ''Offset
makeLenses ''From
makeLenses ''OrderBy
makeLenses ''Select
makeLenses ''Where
makeLenses ''ColConstraint
makeLenses ''ColRef
makeLenses ''Column
makeLenses ''SelectQuery
makeLenses ''SortRef
makeLenses ''Table
makeLenses ''TableReferenceAlias