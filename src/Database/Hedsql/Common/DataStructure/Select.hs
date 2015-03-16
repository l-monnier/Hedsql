{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs                #-}

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

import Database.Hedsql.Drivers.MariaDB.Driver

import Control.Lens

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- View
----------------------------------------

-- | CREATE VIEW query.
data CreateView a = CreateView
    { _viewName   :: String
    , _viewSelect :: SelectWrap a
    }

----------------------------------------
-- Tables
----------------------------------------

{-|
A table reference can be a real table, or a derived table such as
a table join or a sub-query.
Table references are usually used in FROM clauses or joins.
-}
data TableRef a =
      LateralTableRef  (SelectWrap a) (TableRefAs a)
    | SelectTableRef   (SelectWrap a) (TableRefAs a)
    | TableJoinRef     (Join a)       (Maybe (TableRefAs a))
    | TableTableRef    (Table a)      (Maybe (TableRefAs a))

-- | Table reference (table or join) alias. 
data TableRefAs a = TableRefAs
    { _tableRefAliasName    ::  String
    , _tableRefAliasColumns :: [String]
    }

-- | Table definition.
data Table a = Table
    { _tableIfNotExists :: Bool                -- ^ If true the IF NOT EXISTS
                                               --   clause is added.
    , _tableName        :: String              -- ^ Table name.                               
    , _tableCols        :: [ColWrap a]         -- ^ Table columns.
    , _tableConsts      :: [TableConstraint a] -- ^ Table constraints.
    }

-- | Table constraints to be used in CREATE statement.
data TableConstraint a = TableConstraint
    { _tableConstraintName       :: Maybe String
    , _tableConstraintConstraint :: TableConstraintType a
    , _tableConstraintTiming     :: Maybe (ConstraintTiming a)
    }

-- | Table constraints types used in CREATE statement.
data TableConstraintType a =
      ForeignKey                [ColWrap a]         (ForeignKeyClause a)
    | TableConstraintPrimaryKey [ColWrap a ]
    | TableConstraintUnique     [ColWrap a]
    | TableConstraintCheck      (Expression Bool a)

-- | Foreign key clause to be used in a table constraint of a CREATE statement.
data ForeignKeyClause a = ForeignKeyClause
    { _foreignKeyClauseTable  :: Table a
    , _foreignKeyClauseCols   :: [ColWrap a]
    , _foreignKeyMatch        :: Maybe (Match a)
    , _foreignKeyClauseAction :: Maybe (OnAction a)
    }

-- | Foreign key match type.
data Match a =
      Full
    | Partial
    | Simple
      
-- | Timing of a constraint.
data ConstraintTiming a = ConstraintTiming
    { _constraintTimingType  :: ConstraintTimingType a
    , _constraintTimingCheck :: ConstraintTimingCheck a
    }

-- | Type of a timing constraint.
data ConstraintTimingType a =
      Deferable
    | NotDeferable

-- | Timing of a timing constraint.
data ConstraintTimingCheck a =
      InitiallyImmediate
    | InitiallyDeferred

-- | Constraint on a column.
data ColConstraint a = ColConstraint
    { _colConstraintName :: Maybe String
    , _colConstraintType :: ColConstraintType a
    }

-- | Column constraints types.
data ColConstraintType a where
    -- TODO: | Collate
    
    Check   :: Expression Bool a -> ColConstraintType a
    
    -- TODO: their should be a way to constraint the type of a column to the
    --       one of the DEFAULT expression.
    Default :: Expression b a    -> ColConstraintType a
    NotNull ::                      ColConstraintType a
    Null    ::                      ColConstraintType a
    
    -- ^ Primary key. 
    Primary ::
           Bool                 -- ^ If true, the primary key will have an
                                --   AUTOINCREMENT.
        -> ColConstraintType a 
    
    -- ^ Foreign key.     
    Reference ::
           Table a
        -> ColWrap a
        -> Maybe (OnAction a)
        -> ColConstraintType a
    
    Unique :: ColConstraintType a

-- | Column definition used for data queries such as SELECT queries.
data Column b a = Column
    { _colName        :: String
    , _colDataType    :: DataType b a
    , _colConstraints :: [ColConstraint a]
    }

-- | SQL data types.
data DataType b a where
    Bool :: DataType Bool a
     
    -- Types related to time.
    Date :: DataType a Time
   
    -- Textual types.
    Char    :: Int -> DataType Text a
    Varchar :: Int -> DataType Text a
    
    -- Numeric types.
    SmallInt :: DataType Numeric a -- ^ 2 bytes integer
                                   --   - range from -32768 to +32767.
    Integer  :: DataType Numeric a -- ^ 4 bytes integer
                                   --   - range from -2147483648 to +2147483647.
    BigInt   :: DataType Numeric a -- ^ 8 bytes integer -
                                   --   - range from -9223372036854775808
                                   --   to +9223372036854775807.
    
    -- Undefined type.                               
    Undef :: DataType Undefined a
                                   
data DataTypeWrap a where
    DataTypeWrap :: DataType b a -> DataTypeWrap a

-- | Column wrapper "hiding" the types of different columns.
data ColWrap a where
    ColWrap :: Column b a -> ColWrap a

colName :: Lens' (Column b a) String
colName = lens _colName (\col name -> col {_colName = name})

colConstraints :: Lens' (Column b a) [ColConstraint a]
colConstraints =
        lens _colConstraints (\col consts -> col {_colConstraints = consts})  

colDataType :: Lens' (Column b a) (DataType b a)
colDataType = lens _colDataType (\col dType -> col {_colDataType = dType})

colWrapName :: Lens' (ColWrap a ) String
colWrapName =
    lens getter setter
    where
        getter (ColWrap col) = _colName col
        setter (ColWrap col) name = ColWrap $ col {_colName = name}

colWrapConstraints :: Lens' (ColWrap a) [ColConstraint a]
colWrapConstraints =
    lens getter setter
    where
        getter (ColWrap col) = _colConstraints col
        setter (ColWrap col) name = ColWrap $ col {_colConstraints = name}

colWrapDataType :: Lens' (ColWrap a) (DataTypeWrap a)
colWrapDataType =
    lens getter setter
    where
        getter (ColWrap col) = DataTypeWrap $ _colDataType col
        setter (ColWrap col) (DataTypeWrap dType) =
            ColWrap $ col {_colDataType = dType}

{-|
Column definition, which includes a reference to a table for qualified
table names.
-}
data ColDef b a = ColDef
    { _colExpr           :: (Column b a)
    , _colExprTableLabel :: Maybe (TableRef a) -- ^ Qualified column name.
    }

{-|
Generic definition of a column reference used in the SELECT clause of the query.
-}
data ColRef b a = ColRef
    { _colRefExpr  :: Expression b a
    , _colRefLabel :: Maybe String
    }

data ColRefWrap a where
    ColRefWrap :: ColRef b a -> ColRefWrap a

-- | Actions to be performed on foreign keys.
data OnAction a =
      OnDelete (SqlAction a)
    | OnUpdate (SqlAction a)

-- | Action to perform when an entry in a table is updated or deleted.
data SqlAction a =
      Cascade
    | NoAction
    | Restrict
    | SetDefault
    | SetNull

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
data Expression b a where
    
    -- Values
    Value       :: Value b a   -> Expression b a
    Values      :: [Value b a] -> Expression [b] a
    
    -- Column
    ColExpr     :: ColDef b a -> Expression b a
    
    {-|
    * sign. Note: as it is not possible to know the number of columns returned,
    it is assumed that many are.
    -} 
    Joker       :: Expression Undefineds a
    
    -- Select
    SelectExpr :: Select b a -> Expression b a
    
    -- Conditions
    And         :: [Expression Bool a] -> Expression Bool a
    Or          :: [Expression Bool a] -> Expression Bool a
    
    -- Functions
    -- - Boolean functions
    -- ^ BETWEEN. Note: it can also apply for textual values.
    Between ::
            ColRef b a
         -> ColRef b a
         -> ColRef b a
         -> Expression Bool a
         
    -- ^ EXISTS.
    Exists :: ColRef b a -> Expression Bool a                      
    
    -- ^ IS FALSE.
    IsFalse :: ColRef Bool a -> Expression Bool a                     
    
    -- ^ IS NOT FALSE.
    IsNotFalse :: ColRef Bool a -> Expression Bool a                      
    
    -- ^ IS NOT NULL.
    IsNotNull :: ColRef b a -> Expression Bool a                      
    
    -- ^ IS NOT TRUE.
    IsNotTrue :: ColRef Bool a -> Expression Bool a 
    
    -- ^ IS NOT UNKNOWN
    IsNotUnknown :: ColRef b a -> Expression Bool a                      
    
    -- ^ IS NULL.
    IsNull :: ColRef b a -> Expression Bool a
    
    -- ^ IS TRUE.
    IsTrue :: ColRef Bool a -> Expression Bool a
    
    -- ^ IS UNKNOWN
    IsUnknown :: ColRef b a -> Expression Bool a
    
    -- ^ BETWEEN. Note: it can also apply for textual values.                     
    NotBetween ::
            ColRef b a
         -> ColRef b a
         -> ColRef b a
         -> Expression Bool a
    
    -- ^ Equality ("=") operator.
    Equal :: ColRef b a -> ColRef b a ->Expression Bool a           
    
    -- ^ Greater than (">") operator.
    GreaterThan :: ColRef b a -> ColRef b a ->Expression Bool a
    
    -- ^ Greater than or equal to (">=") operator.         
    GreaterThanOrEqTo :: ColRef b a -> ColRef b a ->Expression Bool a          
    
    -- ^ IN. Note: it can be any types but no arrays.
    In :: ColRef b a -> ColRef b a -> Expression Bool a     
    
    -- ^ IS DISTINCT FROM.
    IsDistinctFrom  :: ColRef b a -> ColRef b a -> Expression Bool a          
    
    -- ^ IS NOT DISTINCT FROM.
    IsNotDistinctFrom :: ColRef b a -> ColRef b a -> Expression Bool a
    
    -- ^ LIKE.
    Like :: ColRef Text a -> ColRef Text a -> Expression Bool a            
    
    -- ^ Unequality ("<>") operator.
    NotEqual :: ColRef b a -> ColRef b a -> Expression Bool a           
    
    -- ^ NOT IN. Note: it can be any types but no arrays.
    NotIn :: ColRef b a -> ColRef b a -> Expression Bool a        
    
    -- ^ Smaller than ("<") operator.
    SmallerThan :: ColRef b a -> ColRef b a -> Expression Bool a
              
    -- ^ Smaller than or equal to ("<=") operator.
    SmallerThanOrEqTo ::
           SQLOrd b 
        => ColRef b a
        -> ColRef b a
        -> Expression Bool a
    
    -- - Numeric functions
    -- | Addition ("+") operator.
    Add ::                      
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a
    
    -- | Bitwise AND ("&") operator.
    BitAnd :: 
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a
    
    -- | Bitwise OR ("|") operator.
    BitOr ::
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a
    
    -- | Bitwise shift left  ("<<") operator.
    BitShiftLeft  ::
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a
    
    -- | Bitwise shift right  (">>") operator.
    BitShiftRight ::
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a
    
    -- | Division ("/") operator.
    Divide ::                      
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a
    
    -- | Modulo - remainer - ("%") operator.        
    Modulo ::                      
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a                       
    
    -- | "*" Multiplication operator.
    Multiply ::
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a 
    
    -- | Subtraction "-" operator.
    Substract ::
           ColRef Numeric a
        -> ColRef Numeric a
        -> Expression Numeric a 
    
    -- | COUNT function.
    Count :: ColRef b a -> Expression Numeric a
    
    -- | MAX function. Note: it can also operates on strings in SQL.
    Max :: ColRef b a -> Expression b a
    
    -- | MIN function. Note: it can also operates on strings in SQL.
    Min :: ColRef b a -> Expression b a
    
    -- | SUM function.
    Sum :: ColRef Numeric a -> Expression Numeric a
    
    -- | RANDOM number function.
    Random :: Expression Numeric a
    
    -- - Date functions.
    CurrentDate :: Expression Time a
    
    -- MariaDB specific functions.
    CalcFoundRows :: Expression MariaDB Void
    FoundRows     :: Expression MariaDB Numeric

-- | Expression wrapper "hiding" the types of an expression.
data ExprWrap a where
    ExprWrap :: Expression b a -> ExprWrap a

----------------------------------------
-- VALUES
----------------------------------------

data Value b a where
    BoolVal     :: Bool             -> Value Bool a
    IntVal      :: Int              -> Value Numeric a
    StringVal   :: String           -> Value Text a
    DefaultVal  ::                     Value b a
    NullVal     ::                     Value Undefined a
    Placeholder ::                     Value b a

data ValueWrap a where
    ValueWrap :: Value b a -> ValueWrap a

-- | Numeric value (integer, float, double, etc.)    
data Numeric = Numeric

-- | Types related to time (date, datetime, timestamps, etc.)
data Time = Time

-- | Textual values (strings).
data Text = Text

-- | Value for which no type is defined (typically a NULL value).
data Undefined = Undefined

-- | List of undefined values used for SELECT queries.
data Undefineds = Undefineds

-- | No value returned by an expression.
data Void = Void

-- | Types which can be ordered in SQL.
class SQLOrd a where
instance SQLOrd Bool where
instance SQLOrd Numeric where
instance SQLOrd Text where
instance SQLOrd Time where

-- | Types which can be used as PRIMARY key or for an UNIQUE constraint.
class Uniq a where
instance Uniq Numeric where
instance Uniq Text where
instance Uniq Time where

-- | Note: NULL values can be compared in SQL...
instance SQLOrd Undefined where

----------------------------------------
-- SELECT
----------------------------------------

-- | SELECT query.
data Select b a where
    TSelect :: ColRef b a -> SelectBody a -> Select b a
    USelect :: [ColRefWrap a] -> SelectBody a -> Select Undefineds a

getSelectColRefs :: Select b a -> [ColRefWrap a]
getSelectColRefs (TSelect col _)  = [ColRefWrap col]
getSelectColRefs (USelect cols _) = cols

selectBody :: Lens' (Select b a) (SelectBody a)
selectBody =
    lens getter setter
    where
        getter (TSelect _ body) = body
        getter (USelect _ body) = body
        
        setter (TSelect col _)  body = TSelect col  body
        setter (USelect cols _) body = USelect cols body

-- | Select wrapper.
data SelectWrap a where
    SelectWrap :: Select b a -> SelectWrap a

-- | "Body" of a select query: all the clauses except the columns' selection.
data SelectBody a = SelectBody
    { _selectType    :: Maybe (SelectType a)
    , _fromClause    :: Maybe (From a)
    , _whereClause   :: Maybe (Where a)
    , _groupByClause :: Maybe (GroupBy a)
    , _orderByClause :: Maybe (OrderBy a)
    }

-- | Type of SELECT query (ALL or DISTINCT).
data SelectType a =
     All
   | Distinct
   | DistinctOn [ColRefWrap a]

-- | FROM part of a query.
data From a = From [TableRef a]

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
    }

-- | JOIN clause: ON or USING.
data JoinClause a =
      JoinClauseOn    (Expression Bool a)
    | JoinClauseUsing [ColWrap a]

-- | JOIN on columns.
data JoinTypeCol a =
      FullJoin 
    | LeftJoin
    | InnerJoin
    | RightJoin

-- | JOIN on tables.
data JoinTypeTable a =
      CrossJoin
    | NaturalInnerJoin
    | NaturalLeftJoin
    | NaturalRightJoin
    | NaturalFullJoin

-- | LIMIT clause.
data Limit a = Limit
    { _limitVal :: Int
    }

{- |
WHERE part of the query consisting of a condition. A can be single predicate or
an AND or OR list of conditions.
-}
data Where a = Where (Expression Bool a)

-- | GROUP BY clause.
data GroupBy a = GroupBy
    { _groupByCols :: [ColRefWrap a]
    , _groupByHaving :: Maybe (Having a)
    }

-- | HAVING clause.
data Having a = Having (Expression Bool a)

-- | OFFSET clause.
data Offset a = Offset
    { offsetVal :: Int
    }

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
    }

-- | NULLS FIRST and NULLS LAST parameters for sorting.
data SortNulls a =
      NullsFirst
    | NullsLast

-- | Sorting order.
data SortOrder a =
      Asc
    | Desc

-- | Defines how a given column has to be sorted.
data SortRef a = SortRef
    { _sortRefColRefs :: ColRefWrap a         -- ^ Sorted column references.
    , _sortRefOrder   :: Maybe (SortOrder a)  -- ^ Order (ASC or DESC).
    , _sortRefNulls   :: Maybe (SortNulls a)  -- ^ NULL values first or last.
    }

-- | Combined query such as UNION.
data CombinedQuery a =
      Single       (SelectWrap a)
    | Except       [CombinedQuery a]
    | ExceptAll    [CombinedQuery a]
    | Intersect    [CombinedQuery a]
    | IntersectAll [CombinedQuery a]
    | Union        [CombinedQuery a]
    | UnionAll     [CombinedQuery a]

----------------------------------------
-- Lenses
----------------------------------------

makeLenses ''ColConstraint
makeLenses ''ColDef
makeLenses ''ColRef
makeLenses ''CreateView
makeLenses ''From
makeLenses ''GroupBy
makeLenses ''Join
makeLenses ''OrderBy
makeLenses ''Where
makeLenses ''Limit
makeLenses ''Offset
makeLenses ''SortRef
makeLenses ''Table
makeLenses ''TableConstraint
makeLenses ''TableRefAs
makeLenses ''ConstraintTiming
makeLenses ''ForeignKeyClause
makeLenses ''SelectBody