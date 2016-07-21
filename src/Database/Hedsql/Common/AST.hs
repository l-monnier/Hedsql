{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : Database/Hedsql/Commun/AST.hs
Description : SQL abstract syntax tree.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Abstract syntax tree (AST) of the SQL language.

= Use Database.Hedsql.Common.ASTis relying on GADTs for certain data type to
determine the type of the SQL expressions. For example, a column containing an
integer will have a phantom type "Numeric". By convention, when generic,
this type is always referred as "b".

On top of that, a phantom type is added to all data types. It represents
the database vendor such as PostgreSQL for example. As long as a query
remains valid for all vendors (is not using any vendor specific features)
the phantom type will remain generic. By convention, when generic, this type
is always referred as "a".

It means that a column containing an integer, valid across all database vendors
would have the following signature:
> Column Numeric a

= Wrappers
In SQL lists can contain heterogeneous types, which is not the case of our
favorite language. On top of that, when parsed, the GADTs are not relevant
anymore – they are only used to enforce the construction of coherent queries.

The "hiding" of the GADT is done through "wrappers". For example, the one for
a column is as follow:
@
data ColWrap a where ColWrap :: Column b a -> ColWrap a
@

= Lenses and getters

It is possible to access and manipulate the data of the AST using lenses.
On the other hand, the records fields acessors aren't exported and remain
therefore internal.

Because of the wrappers and the use of GADTs it is unfortunately not possible
to provide lenses for all data types. In such cases, getter are provided
instead.
By convention, they all start with "get" to differentiate them from the lenses.
-}
module Database.Hedsql.Common.AST
    (
      -- * Statement
      Statement(..)

      -- * Language definition

      -- ** Type system

      -- | Type system used in this AST to simulate the SQL types and to enforce
      --   some of the language behaviors at compile type.
      --
      --   Only types specific to SQL are implemented here. It is thus possible
      --   to use Haskell types such as 'Bool', 'Float', 'String', etc.

    , Numeric(Numeric)
    , Aggregate(Aggregate)
    , AggrPred(AggrPred)
    , Time(Time)
    , Undefined(Undefined)
    , Void(Void)
    , SQLOrd
    , Raw
    , SQLUniq

      -- ** Expressions
    , Expression(..)
    , ExprWrap(ExprWrap)

      -- * Data definition

      -- | Operations acting on tables' structure.

      -- ** Components
    , Table(Table)
    , tableName
    , tableCols
    , tableConsts

    , TableConstraint(TableConstraint)
    , tableConstraintName
    , tableConstraintType
    , tableConstraintTiming

    , TableConstraintType(..)

    , ForeignKey(ForeignKey)
    , foreignKeyTable
    , foreignKeyCols
    , foreignKeyMatch
    , foreignKeyAction

    , Match(..)

    , OnAction(..)

    , SqlAction(..)

    , ConstraintTiming(ConstraintTiming)
    , isConstraintDeferable
    , isConstraintImmediate

    , View(View)
    , viewName
    , viewSelect

    , Column(Column)
    , colName
    , colType
    , colConstraints
    , ColWrap(ColWrap)
    , colWrapName
    , colWrapType
    , colWrapConstraints

    , DataType(..)
    , DataTypeWrap(DataTypeWrap)

    , ColConstraint(ColConstraint)
    , colConstraintName
    , colConstraintType

    , ColConstraintType(..)

      -- ** CREATE
    , Create(CreateTable, CreateView)
    , createTableIfNotExists
    , createT
    , createViewIfNotExists
    , createV

      -- ** DROP
    , Drop(DropTable, DropView)
    , dropTIfExists
    , dropT
    , dropVIfExists
    , dropV

      -- * Data manipulation

      -- ** Components

    , TableRef(..)
    , getTableRefAlias
    , getTableRefName
    , TableRefAs(TableRefAs)
    , tableRefAsName
    , tableRefAsCols

    , ColDef(ColDef)
    , colExpr
    , colExprTableLabel
    , ColDefWrap(ColDefWrap)

    , ColRef(ColRef)
    , colRefExpr
    , colRefLabel
    , ColRefWrap(ColRefWrap)

    , Value(..)
    , ValueWrap(ValueWrap)

      -- | Operations acting on data.

      -- ** SELECT
    , Select(..)
    , getSelects
    , setSelects
    , Combination(..)
    , SelectQ(SelectQ)
    , selectType
    , selectCols
    , selectFrom
    , selectWhere
    , selectHaving
    , selectGroupBy
    , selectOrderBy
    , selectLimit
    , selectOffset
    , SelectWrap(..)

      -- *** Selection clause
    , SelectType(..)
    , Selection(..)
    , SelectionWrap(..)
    , getSelectedCols

      -- *** FROM clause
    , From(From)
    , fromTableRefs

    , Join(JoinTable, JoinCol)
    , joinTableType
    , joinTableTable1
    , joinTableTable2
    , joinColType
    , joinColTable1
    , joinColTable2
    , joinColClause
    , JoinClause(..)
    , JoinTypeCol(..)
    , JoinTypeTable(..)

      -- *** WHERE clause
    , Where(Where)
    , whereExpr

      -- *** GROUP BY and HAVING clauses
    , GroupBy(GroupBy)
    , groupByColRefs

    , Having(HavingPred, HavingAggrPred)
    , getHavingExpr

    -- *** ORDER BY, OFFSET and LIMIT clauses
    , OrderBy(OrderBy)
    , orderBySortSpecList
    , SortRef(SortRef)
    , sortRefColRef
    , sortRefOrder
    , sortRefNulls
    , SortOrder(..)
    , SortNulls(..)

    , Offset(Offset)
    , offsetVal

    , Limit(Limit)
    , limitVal

      -- ** INSERT
    , Assignment(Assignment)
    , getAssignCol
    , getAssignExpr

    , Insert(Insert)
    , insertTable
    , insertAssign
    , insertReturning
    , InsertWrap(InsertWrap)

      -- *** RETURNING clause
    , Returning(Returning)
    , ReturningWrap(ReturningWrap)

      -- ** UPDATE
    , Update(Update)
    , updateTable
    , updateAssignments
    , updateWhere
    , updateReturning
    , UpdateWrap(UpdateWrap)

      -- ** DELETE
    , Delete (Delete)
    , deleteTable
    , deleteWhere
    , deleteReturning
    , DeleteWrap(DeleteWrap)

    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Drivers.MariaDB.Driver

import Control.Lens

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- Statement
----------------------------------------

{-|
A statement stands at the top of the AST.
It can represent all the offered options by Hedsql as an eDSL for SQL.
-}
data Statement dbVendor =

      -- | CREATE statement.
      CreateStmt (Create dbVendor)

      -- | DROP statement.
    | DropStmt (Drop dbVendor)

      -- | SELECT query.
    | SelectStmt (SelectWrap dbVendor)

      -- | INSERT statement.
    | InsertStmt (InsertWrap dbVendor)

      -- | UPDATE statement.
    | UpdateStmt (UpdateWrap dbVendor)

      -- | DELETE statement.
    | DeleteStmt (DeleteWrap dbVendor)

      -- | Combination of multiple statements (which shall be executed one
      --   after the other).
    | Statements [Statement dbVendor]

----------------------------------------
-- Language elements
----------------------------------------

--------------------
-- Type system
--------------------

-- | Generic numeric value (integer, float, double, etc.)
data Numeric = Numeric

{-|
An aggregate value is a numeric value returned by one of the aggregate
function defined in SQL – COUNT, MAX, MIN, SUM, etc.

To the contrary of most languages, in SQL the result of an aggregate function
does not behave as one of its individual element.
The two following SELECT queries succeed in PostgreSQL:
> SELECT SUM(0)
> SELECT * FROM "Table" WHERE 1 > 0

But not this one:
> SELECT * FROM "Table" WHERE 1 > SUM(0)

In Hedsql, this behavior is reproduced by using this dedicate type 'Aggregate'.

This design choice implies that the SELECT query has the effect to coerce
aggregates to the type of its elements, as the following query is valid SQL in
PostgreSQL. If it ought to be remains however another question!
> SELECT * FROM "Table" WHERE 1 > (SELECT SUM(0))
-}
data Aggregate = Aggregate

{-|
The 'AggrPred' type represents a predicate which contains an aggregate function
– COUNT, MAX, MIN, SUM, etc.

The particularity of this kind of predicates is that they cannot be used in
a WHERE clause. For example, the following query is incorrect:
> SELECT * FROM "Table" WHERE 1 > SUM(0)

However, the ones below are (in PostgreSQL):
> SELECT * FROM "Table" GROUP BY ("id") HAVING 1 > SUM(0)
> SELECT 1 > SUM(0)
> SELECT * FROM "Table" WHERE (SELECT 1 > SUM(0))

Concerning the last one, please refer to the documentation of the 'Aggregate'
type.
-}
data AggrPred = AggrPred

-- | Types related to time (date, datetime, timestamps, etc.)
data Time = Time

-- | Value for which no type is defined (typically a NULL value).
data Undefined = Undefined

-- | No value returned by an expression.
data Void = Void

-- | Types which can be ordered in SQL.
class SQLOrd dbVendor where
instance SQLOrd Bool where
instance SQLOrd Double where
instance SQLOrd Float where
instance SQLOrd Int where
instance SQLOrd Numeric where
instance SQLOrd String where
instance SQLOrd Time where

-- | Note: NULL values can be compared in SQL...
instance SQLOrd Undefined where

{-|
Values which can be used "raw" and don't need to be quoted when used in a
statement.
-}
class Raw dbVendor where
instance Raw Bool where
instance Raw Double where
instance Raw Float where
instance Raw Numeric where
instance Raw Int where

-- | Types which can be used as PRIMARY key or for an UNIQUE constraint.
class SQLUniq dbVendor where
instance SQLUniq Int where
instance SQLUniq Numeric where
instance SQLUniq String where
instance SQLUniq Time where

--------------------
-- Expressions
--------------------

{-|
Generic expression which can then be used in a SELECT or WHERE part.
An expression can either be:
- the name of a column
- a function
- a select query
- a value.
-}
data Expression colType dbVendor where

    -- Values
    Value       :: Value colType dbVendor   -> Expression colType dbVendor
    Values      :: [Value colType dbVendor] -> Expression [colType] dbVendor

    -- Column
    ColExpr     :: ColDef colType dbVendor -> Expression colType dbVendor

    {-|
    * sign. Note: as it is not possible to know the number of columns returned,
    it is assumed that many are.
    -}
    Joker       :: Expression [Undefined] dbVendor

    -- Select
    SelectExpr :: Select colType dbVendor -> Expression colType dbVendor

    -- Conditions
    And ::
           Expression Bool dbVendor -- ^ Left part of the 'And'.
        -> Expression Bool dbVendor -- ^ Right part of the 'And'.
        -> Bool                     -- ^ If 'True' the 'And' is encapsulated in
                                    --   Parenthesis.
        -> Expression Bool dbVendor
    Or  ::
           Expression Bool dbVendor -- ^ Left part of the 'Or'.
        -> Expression Bool dbVendor -- ^ Right part of the 'Or'.
        -> Bool
        -> Expression Bool dbVendor -- ^ If 'True' the 'Or' is encapsulated in
                                    --   Parenthesis.

    -- Functions
    -- - Boolean functions
    -- | BETWEEN. Note: it can also apply for textual values.
    Between ::
           ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression Bool dbVendor

    -- | EXISTS.
    Exists :: ColRef colType dbVendor -> Expression Bool dbVendor

    -- | IS FALSE.
    IsFalse :: ColRef Bool dbVendor -> Expression Bool dbVendor

    -- | IS NOT FALSE.
    IsNotFalse :: ColRef Bool dbVendor -> Expression Bool dbVendor

    -- | IS NOT NULL.
    IsNotNull :: ColRef colType dbVendor -> Expression Bool dbVendor

    -- | IS NOT TRUE.
    IsNotTrue :: ColRef Bool dbVendor -> Expression Bool dbVendor

    -- | IS NOT UNKNOWN
    IsNotUnknown :: ColRef colType dbVendor -> Expression Bool dbVendor

    -- | IS NULL.
    IsNull :: ColRef colType dbVendor -> Expression Bool dbVendor

    -- | IS TRUE.
    IsTrue :: ColRef Bool dbVendor -> Expression Bool dbVendor

    -- | IS UNKNOWN.
    IsUnknown :: ColRef colType dbVendor -> Expression Bool dbVendor

    -- | BETWEEN. Note: it can also apply for textual values.
    NotBetween ::
            ColRef b dbVendor
         -> ColRef b dbVendor
         -> ColRef b dbVendor
         -> Expression Bool dbVendor

    -- | Equality ("=") operator.
    Equal ::
           ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression Bool dbVendor

    -- | Greater than (">") operator.
    GreaterThan ::
           ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression Bool dbVendor

    -- | Greater than or equal to (">=") operator.
    GreaterThanOrEqTo ::
           ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression Bool dbVendor

    -- | IN.
    In ::
           ColRef colType dbVendor
        -> ColRef [colType] dbVendor
        -> Expression Bool dbVendor

    -- | IS DISTINCT FROM.
    IsDistinctFrom  ::
           ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression Bool dbVendor

    -- | IS NOT DISTINCT FROM.
    IsNotDistinctFrom ::
           ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression Bool dbVendor

    -- | LIKE.
    Like ::
           ColRef String dbVendor
        -> ColRef String dbVendor
        -> Expression Bool dbVendor

    -- | Unequality ("<>") operator.
    NotEqual ::
          ColRef colType dbVendor
       -> ColRef colType dbVendor
       -> Expression Bool dbVendor

    -- | NOT IN. Note: it can be any types but no arrays.
    NotIn ::
          ColRef colType dbVendor
       -> ColRef [colType] dbVendor
       -> Expression Bool dbVendor

    -- | Smaller than ("<") operator.
    SmallerThan ::
          ColRef colType dbVendor
       -> ColRef colType dbVendor
       -> Expression Bool dbVendor

    -- | Smaller than or equal to ("<=") operator.
    SmallerThanOrEqTo ::
           SQLOrd colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression Bool dbVendor

    -- - Numeric functions
    -- | Addition ("+") operator.
    Add ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | Bitwise AND ("&") operator.
    BitAnd ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | Bitwise OR ("|") operator.
    BitOr ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | Bitwise shift left  ("<<") operator.
    BitShiftLeft  ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | Bitwise shift right  (">>") operator.
    BitShiftRight ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | Division ("/") operator.
    Divide ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | Modulo - remainer - ("%") operator.
    Modulo ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | "*" Multiplication operator.
    Multiply ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | Subtraction "-" operator.
    Substract ::
           Num colType
        => ColRef colType dbVendor
        -> ColRef colType dbVendor
        -> Expression colType dbVendor

    -- | COUNT function.
    Count :: ColRef colType dbVendor -> Expression Int dbVendor

    -- | MAX function. Note: it can also operates on strings in SQL.
    Max :: Num colType => ColRef colType dbVendor -> Expression colType dbVendor

    -- | MIN function. Note: it can also operates on strings in SQL.
    Min :: Num colType => ColRef colType dbVendor -> Expression colType dbVendor

    -- | SUM function.
    Sum :: Num colType => ColRef colType dbVendor -> Expression colType dbVendor

    -- | RANDOM number function.
    Random :: Num colType => Expression colType dbVendor

    {-|
    Function returning the primary key of the last inserted row.
    -}
    LastInsertId :: Expression colType dbVendor

    -- - Date functions.

    -- | CURRENT DATE.
    CurrentDate :: Expression Time dbVendor

    -- MariaDB specific functions.
    CalcFoundRows :: Expression MariaDB Void
    FoundRows     :: Expression MariaDB Int

-- | Expression wrapper "hiding" the types of an expression.
data ExprWrap dbVendor where
    ExprWrap :: Expression colType dbVendor -> ExprWrap dbVendor

----------------------------------------
-- Data definition
----------------------------------------

--------------------
-- Components
--------------------

-- | Representation of table.
data Table dbVendor = Table
    {
      -- | Table name.
      _tableName :: String

      -- | Table's columns.
    , _tableCols :: [ColWrap dbVendor]

      -- | Table's constraints.
    , _tableConsts :: [TableConstraint dbVendor]
    }

-- | Constraints to be applied at the table level.
data TableConstraint dbVendor = TableConstraint
    {
      -- | Optional: name of the constraint.
      _tableConstraintName :: Maybe String

      -- | Kind of constraint.
    , _tableConstraintType :: TableConstraintType dbVendor

      -- | Timing of the constraint.
    , _tableConstraintTiming :: Maybe (ConstraintTiming dbVendor)
    }

-- | Table constraints types used in CREATE statement.
data TableConstraintType dbVendor =

      -- | Foreign key.
      TCForeignKey [ColWrap dbVendor] (ForeignKey dbVendor)

      -- | Primary key.
    | TCPrimaryKey [ColWrap dbVendor ]

      -- | Unique.
    | TCUnique [ColWrap dbVendor]

      -- | Check.
    | TCCheck (Expression Bool dbVendor)

-- | Foreign key clause to be used in a table constraint of a CREATE statement.
data ForeignKey dbVendor = ForeignKey
    {
      -- | Table on witch the foreign key applies.
      _foreignKeyTable :: Table dbVendor

      -- | Columns on witch the foreign key applies.
    , _foreignKeyCols :: [ColWrap dbVendor]

      -- | Kind of match (FULL, PARTIAL or SIMPLE).
    , _foreignKeyMatch :: Maybe (Match dbVendor)

      -- | ON clauses (DELETE, UPDATE).
    , _foreignKeyAction :: Maybe (OnAction dbVendor)
    }

-- | Foreign key match type.
data Match dbVendor =
      Full
    | Partial
    | Simple

-- | Actions to be performed on foreign keys.
data OnAction dbVendor =
      OnDelete (SqlAction dbVendor)
    | OnUpdate (SqlAction dbVendor)

-- | Action to perform when an entry in a table is updated or deleted.
data SqlAction dbVendor =
      Cascade
    | NoAction
    | Restrict
    | SetDefault
    | SetNull

-- | Timing of a constraint.
data ConstraintTiming dbVendor = ConstraintTiming
    {
      -- | If 'True' the constraint is deferable.
      --   If 'False' the constraint is not deferable.
      _isConstraintDeferable  :: Bool

      -- | If 'True' the constraint is immediate.
      --   If 'False' the constraint is initially deferred.
    , _isConstraintImmediate :: Bool
    }

-- | Representation of view.
data View dbVendor = View
    {
      -- | View name.
      _viewName :: String

      -- | SELECT query generating the view.
    , _viewSelect :: SelectWrap dbVendor
    }

-- | A column in a table.
data Column colType dbVendor = Column
    {
      -- | Name.
      _colName :: String

      -- | Data type.
    , _colType :: DataType colType dbVendor

      -- | Constraints.
    , _colConstraints :: [ColConstraint dbVendor]
    }

-- | Column wrapper "hiding" the types "b" of different columns.
data ColWrap dbVendor where
    ColWrap :: Column colType dbVendor -> ColWrap dbVendor

colWrapName :: Lens' (ColWrap dbVendor ) String
colWrapName =
    lens getter setter
    where
        getter (ColWrap col) = _colName col
        setter (ColWrap col) name = ColWrap $ col {_colName = name}

colWrapType :: Lens' (ColWrap dbVendor) (DataTypeWrap dbVendor)
colWrapType =
    lens getter setter
    where
        getter (ColWrap col) = DataTypeWrap $ _colType col
        setter (ColWrap col) (DataTypeWrap dType) =
            ColWrap $ col {_colType = dType}

colWrapConstraints :: Lens' (ColWrap dbVendor) [ColConstraint dbVendor]
colWrapConstraints =
    lens getter setter
    where
        getter (ColWrap col) = _colConstraints col
        setter (ColWrap col) name = ColWrap $ col {_colConstraints = name}

-- | SQL data types. Used to define the type of a column.
data DataType colType dbVendor where

    Bool :: DataType Bool dbVendor

    -- Types related to time.
    Date :: DataType dbVendor Time -- TODO: inspect, this is probably wrong!

    -- Textual types.
    Char    :: Int -> DataType String dbVendor
    Varchar :: Int -> DataType String dbVendor

    -- Numeric types.

    -- | 2 bytes integer – range from -32768 to +32767.
    SmallInt :: DataType Int dbVendor

    -- | 4 bytes integer – range from -2147483648 to +2147483647.
    Integer  :: DataType Int dbVendor -- TODO: change the name to Int.

    -- | 8 bytes integer
    -- – range from -9223372036854775808 to +9223372036854775807.
    BigInt   :: DataType Int dbVendor

    -- Undefined type.
    Undef :: DataType Undefined dbVendor

-- | Data type wrapper "hidding" the "b" type.
data DataTypeWrap dbVendor where
    DataTypeWrap :: DataType colType dbVendor -> DataTypeWrap dbVendor

-- | Constraint on a column.
data ColConstraint dbVendor = ColConstraint
    {
      -- | Optional: name of the constraint.
      _colConstraintName :: Maybe String

      -- | Type of the constraint.
    , _colConstraintType :: ColConstraintType dbVendor
    }

-- | Column constraints types.
data ColConstraintType dbVendor where
    -- TODO: | Collate

    -- | CHECK.
    Check :: Expression Bool dbVendor -> ColConstraintType dbVendor

    -- | DEFAULT (value).
    Default :: Expression colType dbVendor -> ColConstraintType dbVendor

    -- | NOT NULL.
    NotNull :: ColConstraintType dbVendor

    -- | NULL.
    Null :: ColConstraintType dbVendor

    -- ^ Primary key.
    Primary ::
           -- | If 'True', the primary key will have an AUTOINCREMENT.
           Bool
        -> ColConstraintType dbVendor

    -- ^ Foreign key.
    Reference :: ForeignKey dbVendor -> ColConstraintType dbVendor

    -- | Unique.
    Unique :: ColConstraintType dbVendor

--------------------
-- CREATE
--------------------

-- | CREATE statement.
data Create dbVendor =

      -- | CREATE TABLE statement.
      CreateTable
        {
          -- | If 'True' an IF NOT EXISTS clause is added.
          _createTableIfNotExists :: Bool

          -- | Table to create.
        , _createT :: Table dbVendor
        }

      -- | CREATE VIEW statement.
    | CreateView
        {
          -- | If true an IF NOT EXISTS clause is added.
          _createViewIfNotExists :: Bool

          -- | View to create.
        , _createV :: View dbVendor
        }

--------------------
-- DROP
--------------------

-- | DROP statement.
data Drop dbVendor =

      -- | DROP TABLE statement.
      DropTable
        {
          -- | If true, an IF EXISTS clause is added.
          _dropTIfExists :: Bool

          -- | Table to delete.
        , _dropT :: Table dbVendor
        }
      -- | DROP VIEW statement.
    | DropView
        {
          -- | If true, an IF EXISTS clause is added.
          _dropVIfExists :: Bool

        , _dropV :: View dbVendor
        }

----------------------------------------
-- Data manipulation
----------------------------------------

--------------------
-- Components
--------------------

{-|
A table reference can be a real table, or a derived table such as
a table join or a sub-query.
Table references are usually used in FROM clauses and joins.
-}
data TableRef dbVendor =

      -- | Reference to a table.
      TableRef (Table dbVendor) (Maybe (TableRefAs dbVendor))

      -- | Reference to a join.
    | JoinRef (Join dbVendor) (Maybe (TableRefAs dbVendor))

      -- | Reference to a lateral join.
    | LateralRef (SelectWrap dbVendor) (TableRefAs dbVendor)

      -- | Reference to a SELECT.
    | SelectRef (SelectWrap dbVendor) (TableRefAs dbVendor)

{-|
Return the alias of a table reference if there is one.
Otherwise return Nothing.
-}
getTableRefAlias :: TableRef dbVendor -> Maybe (TableRefAs dbVendor)
getTableRefAlias (LateralRef _ ref) = Just ref
getTableRefAlias (SelectRef  _ ref) = Just ref
getTableRefAlias (JoinRef    _ ref) =      ref
getTableRefAlias (TableRef   _ ref) =      ref

{-|
Return the name of a table reference.
If an alias exists: use the name of this alias.

Otherwise, if this is a table, use that table's name. Else, this is a join
without alias clause: return the names of the two table references
separated by "_". For examples: "Table1_Table2".
-}
getTableRefName :: TableRef dbVendor -> String
getTableRefName ref = maybe name _tableRefAsName $ getTableRefAlias ref
    where
        name = case ref of
            JoinRef (JoinTable _ table1 table2) _ ->
                   getTableRefName table1
                ++ "_"
                ++ getTableRefName table2
            JoinRef (JoinCol _ table1 table2 _) _ ->
                   getTableRefName table1
                ++ "_"
                ++ getTableRefName table2
            TableRef t _ ->
                _tableName t
            _ ->
                error "This is a bug, this pattern shouldn't been reached!"

-- | Table reference (table or join) alias.
data TableRefAs dbVendor = TableRefAs
    {
      -- | Name of the table alias.
      _tableRefAsName ::  String

      -- | Names of the columns aliases.
    , _tableRefAsCols :: [String]
    }

{-|
Column definition, which includes a reference to a table for qualified
column names.
-}
data ColDef colType dbVendor = ColDef
    {
      -- | Column.
      _colExpr :: (Column colType dbVendor)

      -- | Table which will be used for the qualified column name.
    , _colExprTableLabel :: Maybe (TableRef dbVendor)
    }

-- | Column definition wrapper "hidding" the type "b".
data ColDefWrap dbVendor where
    ColDefWrap :: ColDef colType dbVendor -> ColDefWrap dbVendor

{-|
Generic definition of a column reference used in SELECT queries or in the WHERE
clause of UPDATE statements.
-}
data ColRef colType dbVendor = ColRef
    {
      -- | Expression of the column reference
      --   (which is not necesserely a column).
      _colRefExpr  :: Expression colType dbVendor

      -- | Label used to reference the column reference (AS).
    , _colRefLabel :: Maybe String
    }

-- | Column reference wrapper "hidding" the type "b".
data ColRefWrap dbVendor where
    ColRefWrap :: ColRef colType dbVendor -> ColRefWrap dbVendor

-- | Values which can be used in data manipulation statements.
data Value colType dbVendor where

    -- | Boolean value.
    BoolVal :: Bool -> Value Bool dbVendor

    -- | Numeric value.
    NumericVal ::
          (Show colType, Num colType)
        => colType
        -> Value Numeric dbVendor

    -- | Float value.
    FloatVal :: Float -> Value Float dbVendor

    -- | Double value.
    DoubleVal :: Double -> Value Double dbVendor

    -- | Integer value.
    IntVal :: Int -> Value Int dbVendor

    -- | String value.
    StringVal :: String -> Value String dbVendor

    -- | Value of generic type which does not need to be quoted.
    GenVal :: (Raw c, Show c) => c -> Value colType dbVendor

    -- | Value of generic type which needs must quoted.
    GenQVal :: String -> Value colType dbVendor

    -- | Default value (for INSERT, UPDATE or CREATE TABLE statements).
    DefaultVal  :: Value colType dbVendor

    -- Null values.

    -- | NULL value of generic type.
    NullVal :: Value colType dbVendor

    -- Placeholders

    -- | Placeholder for a value of generic type.
    Placeholder :: Value colType dbVendor

    -- | Placeholder for a boolean value.
    PlaceBool :: Value Bool dbVendor

    -- | Placeholder for a numeric value.
    PlaceNum :: Value Numeric dbVendor

    -- | Placeholder for a floating number value.
    PlaceFloat :: Value Float dbVendor

    -- | Placeholder for a double precision number value.
    PlaceDouble :: Value Double dbVendor

    -- | Placeholder for a integer value.
    PlaceInt :: Value Int dbVendor

    -- | Placeholder for a string value.
    PlaceString :: Value String dbVendor

-- | Value wrapper to hide the type "b".
data ValueWrap a where
    ValueWrap :: Value colType dbVendor -> ValueWrap dbVendor

--------------------
-- SELECT
--------------------

-- | SELECT statement.
data Select colType dbVendor =

      -- | SELECT query.
      Single (SelectQ colType dbVendor)

      -- | Combined query such as UNION.
    | Combined (Combination dbVendor) [Select colType dbVendor]

-- | Set a specific clause of all select queries of a SELECT statement.
setSelects ::

       -- | Lens.
       ASetter (SelectQ colType dbVendor) (SelectQ colType dbVendor) d c

       -- | Value to set.
    -> c

       -- | Original SELECT query.
    -> Select colType dbVendor

       -- | SELECT query with the newly set value.
    -> Select colType dbVendor
setSelects l val query =
    case query of
        Single sq -> Single $ set l val sq
        Combined combi sqs -> Combined combi $ map (setSelects l val) sqs

-- | Return the select queries of a SELECT statement.
getSelects :: Select colType dbVendor -> [SelectQ colType dbVendor]
getSelects (Single s) = [s]
getSelects (Combined _ selects) = concat $ map getSelects selects

-- | The different possible combinations of SELECT queries.
data Combination dbVendor =
      Except
    | ExceptAll
    | Intersect
    | IntersectAll
    | Union
    | UnionAll

{-|
SELECT query.

Note: some of the clauses of the SELECT query are not mentionned here.
It is because they cannot (or shouldn't) be used without the mention of another
clause.

More concretely, they are:
  - HAVING which is part of the GROUP BY clause;
  - OFFSET and LIMIT part of the ORDER BY clause.
-}
data SelectQ colType dbVendor = SelectQ
    {
      -- | Type of the SELECT (ALL or DISTINCT).
      _selectType :: SelectType dbVendor

      -- | Selected columns
    , _selectCols :: Selection colType dbVendor

      -- | FROM clause.
    , _selectFrom :: Maybe (From dbVendor)

      -- | WHERE clause.
    , _selectWhere :: Maybe (Where dbVendor)

      -- | Having clause.
    , _selectHaving :: Maybe (Having dbVendor)

      -- | GROUP BY clause.
    , _selectGroupBy :: Maybe (GroupBy dbVendor)

      -- | ORDER BY clause.
    , _selectOrderBy :: Maybe (OrderBy dbVendor)

      -- | LIMIT clause.
    , _selectLimit :: Maybe (Limit dbVendor)

      -- | OFFSET clause.
    , _selectOffset :: Maybe (Offset dbVendor)
    }

-- | SELECT statement wrapper.
data SelectWrap dbVendor where
    SelectWrap :: Select colType dbVendor -> SelectWrap dbVendor

-- Selection clause
--------------------

-- | Type of SELECT query (ALL or DISTINCT).
data SelectType dbVendor =
     All
   | Distinct
   | DistinctOn [ColRefWrap dbVendor]

-- | Columns selected by a SELECT query.
data Selection colType dbVendor where

    -- | A single column.
    --   The returned type is a list representation of the type of that column.
    --   For example, if the column is a Numeric type
    --   then the returned type "b" is [Numeric].
    TSelection :: ColRef colType dbVendor -> Selection colType dbVendor

    -- | Multiple columns of the same type.
    --   The returned type is a list of list representation of the type of
    --   the columns.
    --   For example, if the columns are of type Numeric the return type "b"
    --   will be [[Numeric]].
    TsSelection :: [ColRef colType dbVendor] -> Selection colType dbVendor

    -- | A single column of undefined type.
    --   The returned type is [Undefined].
    USelection  :: ColRefWrap dbVendor -> Selection [Undefined] dbVendor

    -- | Multiple columns of different or undefined types.
    --   The return type is [[Undefined]].
    UsSelection :: [ColRefWrap dbVendor] -> Selection [[Undefined]] dbVendor

    -- | Single column containing an aggregate value (see 'Aggregate' to get
    --   more information).
    --   The returned type is Numeric.
    ASelection :: ColRef Aggregate dbVendor -> Selection Numeric dbVendor

    -- | Multiple columns containing aggregate values (see 'Aggregate' to get
    --   more information).
    --   The returned type is [Numeric].
    AsSelection :: [ColRef Aggregate dbVendor] -> Selection [Numeric] dbVendor

    -- | Single column containing an aggregate predicate
    --   (see 'AggrPred' to get more information).
    --   The returned type is Bool.
    APSelection :: ColRef AggrPred dbVendor -> Selection Bool dbVendor

    -- | Multiple columns containing aggregate predicates
    --   (see 'AggrPred' to get more information).
    --   The returned type is [Bool].
    APsSelection :: [ColRef AggrPred dbVendor] -> Selection [Bool] dbVendor

-- | Selection wrapper.
data SelectionWrap dbVendor where
    SelectionWrap :: Selection colType dbVendor -> SelectionWrap dbVendor

-- | Return the selected columns of a Selection.
getSelectedCols :: Selection colType dbVendor -> [ColRefWrap dbVendor]
getSelectedCols (TSelection   col)  = [ColRefWrap col]
getSelectedCols (TsSelection  cols) = map ColRefWrap cols
getSelectedCols (USelection   col)  = [col]
getSelectedCols (UsSelection  cols) = cols
getSelectedCols (ASelection   col)  = [ColRefWrap col]
getSelectedCols (AsSelection  cols) = map ColRefWrap cols
getSelectedCols (APSelection  col)  = [ColRefWrap col]
getSelectedCols (APsSelection cols) = map ColRefWrap cols

-- FROM clause
--------------------

-- | FROM clause.
data From dbVendor = From {_fromTableRefs :: [TableRef dbVendor]}

-- | A JOIN between two tables.
data Join dbVendor =

      -- | A join which takes only tables references as parameter
      --   – CROSS and NATURAL joins.
      JoinTable
        { _joinTableType   :: JoinTypeTable dbVendor
        , _joinTableTable1 :: TableRef dbVendor
        , _joinTableTable2 :: TableRef dbVendor
        }

      -- | A join which has an ON or USING clause.
    | JoinCol
        { _joinColType   :: JoinTypeCol dbVendor
        , _joinColTable1 :: TableRef dbVendor
        , _joinColTable2 :: TableRef dbVendor
        , _joinColClause :: JoinClause dbVendor
        }

-- | JOIN clause.
data JoinClause dbVendor =

      -- | ON clause.
      JoinClauseOn (Expression Bool dbVendor)

      -- | USING clause.
    | JoinClauseUsing [ColWrap dbVendor]

-- | JOIN between two columns references.
data JoinTypeCol dbVendor =
      FullJoin
    | LeftJoin
    | InnerJoin
    | RightJoin

-- | JOIN between two tables references.
data JoinTypeTable dbVendor =
      CrossJoin
    | NaturalInnerJoin
    | NaturalLeftJoin
    | NaturalRightJoin
    | NaturalFullJoin

-- WHERE clause
--------------------

-- | WHERE clause.
data Where dbVendor = Where
    {
      -- | Predicate.
      _whereExpr :: Expression Bool dbVendor
    }

-- GROUP BY and HAVING clauses
--------------------

-- | GROUP BY clause.
data GroupBy dbVendor = GroupBy
    {
      -- | Columns references of the GROUP BY clause.
      _groupByColRefs :: [ColRefWrap dbVendor]
    }

-- | HAVING clause.
data Having dbVendor =

      -- | Predicate.
      HavingPred (Expression Bool dbVendor)

      -- | Aggregated predicate (see 'AggrPred' to get more information).
    | HavingAggrPred (Expression AggrPred dbVendor)

-- | Return the expression of a HAVING clause in an expression wrapper.
getHavingExpr :: Having dbVendor -> ExprWrap dbVendor
getHavingExpr (HavingPred expr)     = ExprWrap expr
getHavingExpr (HavingAggrPred expr) = ExprWrap expr

-- | ORDER BY clause.
data OrderBy dbVendor = OrderBy
    {
      -- | Definition of the sorting order of the columns.
      _orderBySortSpecList :: [SortRef dbVendor]
    }

-- | Defines how a given column is sorted.
data SortRef dbVendor = SortRef
    {
      -- | Sorted column reference.
      --   A wrapper is used since the type of the column does not matter here.
      --   SQL is able to sort any type, even NULL values.
      _sortRefColRef :: ColRefWrap dbVendor

      -- | Sorting order (ASC or DESC).
    , _sortRefOrder :: Maybe (SortOrder dbVendor)

      -- | NULL values in first or last position.
    , _sortRefNulls :: Maybe (SortNulls dbVendor)
    }

-- | Sorting order (ASC or DESC).
data SortOrder dbVendor =
      Asc
    | Desc

-- | NULLS FIRST and NULLS LAST parameters for sorting the NULL values.
data SortNulls dbVendor =
      NullsFirst
    | NullsLast

-- | OFFSET clause.
data Offset dbVendor = Offset {_offsetVal :: Int}

-- | LIMIT clause.
data Limit dbVendor = Limit {_limitVal :: Int}

--------------------
-- INSERT
--------------------

{-|
A value assigned to a column.
Both must be of the same type (the "b" phantom types must be the same).

Used in UPDATE or INSERT statements.
-}
data Assignment dbVendor where
    Assignment ::
        {
          -- | Column to which the value is assigned.
          _assignCol :: Column colType dbVendor

          -- | Assigned value.
        , _assignExpr :: Expression colType dbVendor
        } -> Assignment dbVendor

getAssignCol :: Assignment dbVendor -> ColWrap dbVendor
getAssignCol (Assignment col _) = ColWrap col

getAssignExpr :: Assignment dbVendor -> ExprWrap dbVendor
getAssignExpr (Assignment _ expr) = ExprWrap expr

-- | INSERT statement.
data Insert colType dbVendor = Insert
    {
      -- | Table in which to insert the values.
      _insertTable :: Table dbVendor

      -- | Column / value assignments for the insertion.
    , _insertAssign :: [Assignment dbVendor]

      -- | Optional RETURNING clause.
    , _insertReturning :: Maybe (Returning colType dbVendor)
    }

-- | INSERT statement wrapper.
data InsertWrap dbVendor where
    InsertWrap :: Insert colType dbVendor -> InsertWrap dbVendor

-- RETURNING clause
--------------------

{-|
RETURNING clause.

This is specific to PostgreSQL for INSERT, UPDATE and DELETE statements.
It can be used in MariaDB for DELETE statements only.

It does not exists in SQLite.
-}
data Returning colType dbVendor = Returning (Selection colType dbVendor)

-- | RETURNING clause wrapper.
data ReturningWrap dbVendor where
    ReturningWrap :: Returning colType dbVendor -> ReturningWrap dbVendor

--------------------
-- UPDATE
--------------------

-- | UPDATE statement.
data Update colType dbVendor = Update
    {
      -- | Table to update.
      _updateTable :: Table dbVendor

      -- | Values to update in the specified columns.
    , _updateAssignments :: [Assignment dbVendor]

      -- | Optional WHERE clause.
    , _updateWhere :: Maybe (Where dbVendor)

      -- | Optional RETURNING clause.
    , _updateReturning :: Maybe (Returning colType dbVendor)
    }

-- | UPDATE statement wrapper.
data UpdateWrap dbVendor where
    UpdateWrap :: Update colType dbVendor -> UpdateWrap dbVendor

--------------------
-- DELETE
--------------------

-- | DELETE statement.
data Delete colType dbVendor = Delete
    {
      -- | Table to delete.
      _deleteTable :: Table dbVendor

      -- | Optional WHERE clause.
    , _deleteWhere :: Maybe (Where dbVendor)

      -- | Optional RETURNING clause.
    , _deleteReturning :: Maybe (Returning colType dbVendor)
    }

-- | DELETE statement wrapper.
data DeleteWrap dbVendor where
    DeleteWrap :: Delete colType dbVendor -> DeleteWrap dbVendor

----------------------------------------
-- Lenses
----------------------------------------

makeLenses ''Table
makeLenses ''TableConstraint
makeLenses ''ForeignKey
makeLenses ''ConstraintTiming
makeLenses ''Column
makeLenses ''ColConstraint
makeLenses ''View
makeLenses ''Create
makeLenses ''Drop

makeLenses ''TableRef
makeLenses ''TableRefAs
makeLenses ''ColDef
makeLenses ''ColRef
makeLenses ''Select
makeLenses ''SelectQ
makeLenses ''From
makeLenses ''Join
makeLenses ''Where
makeLenses ''GroupBy
makeLenses ''OrderBy
makeLenses ''SortRef
makeLenses ''Offset
makeLenses ''Limit
makeLenses ''Insert
makeLenses ''Update
makeLenses ''Delete
