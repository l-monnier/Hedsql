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

= Use Database.Hedsql.Common.ASTis relying on GADTs for certain data type to determine the type of
the SQL expressions. For example, a column containing an integer will have
a phantom type "Numeric". By convention, when generic, this type
is always referred as "b".

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
to provide lenses for all data types. In such cases, getter are provided instead.
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
    , setSelect
    , Combination(..)
    , SelectQ(SelectQ)
    , selectType
    , selectCols
    , selectFrom
    , selectWhere
    , selectGroupBy
    , selectOrderBy
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
    , groupByHaving
    
    , Having(HavingPred, HavingAggrPred)
    , getHavingExpr
    
    -- *** ORDER BY, OFFSET and LIMIT clauses
    , OrderBy(OrderBy)
    , orderByCols
    , orderByOffset
    , orderByLimit
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
    
      -- ** UPDATE
    , Update(Update)
    , updateTable
    , updateAssignments
    , updateWhere
     
      -- ** DELETE
    , Delete (Delete)
    , deleteTable
    , deleteWhere
    
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
data Statement a =
    
      -- | CREATE statement.
      CreateStmt (Create a)
    
      -- | DROP statement.
    | DropStmt (Drop a)
    
      -- | SELECT query.
    | SelectStmt (SelectWrap a)
    
      -- | INSERT statement.
    | InsertStmt (Insert a)
    
      -- | UPDATE statement.
    | UpdateStmt (Update a)
    
      -- | DELETE statement.
    | DeleteStmt (Delete a)
    
      -- | Combination of multiple statements (which shall be executed one
      --   after the other).
    | Statements [Statement a]

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
class SQLOrd a where
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
class Raw a where
instance Raw Bool where
instance Raw Double where
instance Raw Float where
instance Raw Numeric where
instance Raw Int where

-- | Types which can be used as PRIMARY key or for an UNIQUE constraint.
class SQLUniq a where
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
    Joker       :: Expression [Undefined] a
    
    -- Select
    SelectExpr :: Select b a -> Expression b a
    
    -- Conditions
    And         :: [Expression Bool a] -> Expression Bool a
    Or          :: [Expression Bool a] -> Expression Bool a
    
    -- Functions
    -- - Boolean functions
    -- | BETWEEN. Note: it can also apply for textual values.
    Between ::
            ColRef b a
         -> ColRef b a
         -> ColRef b a
         -> Expression Bool a
         
    -- | EXISTS.
    Exists :: ColRef b a -> Expression Bool a                      
    
    -- | IS FALSE.
    IsFalse :: ColRef Bool a -> Expression Bool a                     
    
    -- | IS NOT FALSE.
    IsNotFalse :: ColRef Bool a -> Expression Bool a                      
    
    -- | IS NOT NULL.
    IsNotNull :: ColRef b a -> Expression Bool a                      
    
    -- | IS NOT TRUE.
    IsNotTrue :: ColRef Bool a -> Expression Bool a 
    
    -- | IS NOT UNKNOWN
    IsNotUnknown :: ColRef b a -> Expression Bool a                      
    
    -- | IS NULL.
    IsNull :: ColRef b a -> Expression Bool a
    
    -- | IS TRUE.
    IsTrue :: ColRef Bool a -> Expression Bool a
    
    -- | IS UNKNOWN.
    IsUnknown :: ColRef b a -> Expression Bool a
    
    -- | BETWEEN. Note: it can also apply for textual values.                     
    NotBetween ::
            ColRef b a
         -> ColRef b a
         -> ColRef b a
         -> Expression Bool a
    
    -- | Equality ("=") operator.
    Equal :: ColRef b a -> ColRef b a ->Expression Bool a           
    
    -- | Greater than (">") operator.
    GreaterThan :: ColRef b a -> ColRef b a ->Expression Bool a
    
    -- | Greater than or equal to (">=") operator.         
    GreaterThanOrEqTo :: ColRef b a -> ColRef b a ->Expression Bool a          
    
    -- | IN.
    In :: ColRef b a -> ColRef [b] a -> Expression Bool a     
    
    -- | IS DISTINCT FROM.
    IsDistinctFrom  :: ColRef b a -> ColRef b a -> Expression Bool a          
    
    -- | IS NOT DISTINCT FROM.
    IsNotDistinctFrom :: ColRef b a -> ColRef b a -> Expression Bool a
    
    -- | LIKE.
    Like :: ColRef String a -> ColRef String a -> Expression Bool a            
    
    -- | Unequality ("<>") operator.
    NotEqual :: ColRef b a -> ColRef b a -> Expression Bool a           
    
    -- | NOT IN. Note: it can be any types but no arrays.
    NotIn :: ColRef b a -> ColRef [b] a -> Expression Bool a        
    
    -- | Smaller than ("<") operator.
    SmallerThan :: ColRef b a -> ColRef b a -> Expression Bool a
              
    -- | Smaller than or equal to ("<=") operator.
    SmallerThanOrEqTo ::
           SQLOrd b 
        => ColRef b a
        -> ColRef b a
        -> Expression Bool a
    
    -- - Numeric functions
    -- | Addition ("+") operator.
    Add ::
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a
    
    -- | Bitwise AND ("&") operator.
    BitAnd :: 
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a
    
    -- | Bitwise OR ("|") operator.
    BitOr ::
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a
    
    -- | Bitwise shift left  ("<<") operator.
    BitShiftLeft  ::
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a
    
    -- | Bitwise shift right  (">>") operator.
    BitShiftRight ::
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a
    
    -- | Division ("/") operator.
    Divide ::                      
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a
    
    -- | Modulo - remainer - ("%") operator.        
    Modulo ::                      
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a                       
    
    -- | "*" Multiplication operator.
    Multiply ::
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a 
    
    -- | Subtraction "-" operator.
    Substract ::
           Num b                     
        => ColRef b a
        -> ColRef b a
        -> Expression b a 
    
    -- | COUNT function.
    Count :: ColRef b a -> Expression Int a
    
    -- | MAX function. Note: it can also operates on strings in SQL.
    Max :: Num b => ColRef b a -> Expression b a
    
    -- | MIN function. Note: it can also operates on strings in SQL.
    Min :: Num b => ColRef b a -> Expression b a
    
    -- | SUM function.
    Sum :: Num b => ColRef b a -> Expression b a
    
    -- | RANDOM number function.
    Random :: Num b => Expression b a
    
    -- - Date functions.
    
    -- | CURRENT DATE.
    CurrentDate :: Expression Time a
    
    -- MariaDB specific functions.
    CalcFoundRows :: Expression MariaDB Void
    FoundRows     :: Expression MariaDB Int

-- | Expression wrapper "hiding" the types of an expression.
data ExprWrap a where
    ExprWrap :: Expression b a -> ExprWrap a

----------------------------------------
-- Data definition
----------------------------------------

--------------------
-- Components
--------------------

-- | Representation of table.
data Table a = Table
    {                                            
      -- | Table name.  
      _tableName :: String
    
      -- | Table's columns.                        
    , _tableCols :: [ColWrap a]
    
      -- | Table's constraints. 
    , _tableConsts :: [TableConstraint a]
    }

-- | Constraints to be applied at the table level.
data TableConstraint a = TableConstraint
    { 
      -- | Optional: name of the constraint.
      _tableConstraintName :: Maybe String
      
      -- | Kind of constraint.
    , _tableConstraintType :: TableConstraintType a
    
      -- | Timing of the constraint.
    , _tableConstraintTiming :: Maybe (ConstraintTiming a)
    }

-- | Table constraints types used in CREATE statement.
data TableConstraintType a =

      -- | Foreign key.
      TCForeignKey [ColWrap a] (ForeignKey a)
      
      -- | Primary key.
    | TCPrimaryKey [ColWrap a ]
    
      -- | Unique.
    | TCUnique [ColWrap a]
      
      -- | Check.
    | TCCheck (Expression Bool a)

-- | Foreign key clause to be used in a table constraint of a CREATE statement.
data ForeignKey a = ForeignKey
    {
      -- | Table on witch the foreign key applies.
      _foreignKeyTable :: Table a
      
      -- | Columns on witch the foreign key applies.
    , _foreignKeyCols :: [ColWrap a]
    
      -- | Kind of match (FULL, PARTIAL or SIMPLE).
    , _foreignKeyMatch :: Maybe (Match a)
    
      -- | ON clauses (DELETE, UPDATE).
    , _foreignKeyAction :: Maybe (OnAction a)
    }

-- | Foreign key match type.
data Match a =
      Full
    | Partial
    | Simple

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
   
-- | Timing of a constraint.
data ConstraintTiming a = ConstraintTiming
    { 
      -- | If 'True' the constraint is deferable.
      --   If 'False' the constraint is not deferable.
      _isConstraintDeferable  :: Bool
    
      -- | If 'True' the constraint is immediate.
      --   If 'False' the constraint is initially deferred.
    , _isConstraintImmediate :: Bool
    }

-- | Representation of view.
data View a = View
    { 
      -- | View name.
      _viewName :: String
      
      -- | SELECT query generating the view.
    , _viewSelect :: SelectWrap a
    }

-- | A column in a table.
data Column b a = Column
    {
      -- | Name.
      _colName :: String
      
      -- | Data type.
    , _colType :: DataType b a
    
      -- | Constraints.
    , _colConstraints :: [ColConstraint a]
    }

-- | Column wrapper "hiding" the types "b" of different columns.
data ColWrap a where ColWrap :: Column b a -> ColWrap a

colWrapName :: Lens' (ColWrap a ) String
colWrapName =
    lens getter setter
    where
        getter (ColWrap col) = _colName col
        setter (ColWrap col) name = ColWrap $ col {_colName = name}

colWrapType :: Lens' (ColWrap a) (DataTypeWrap a)
colWrapType =
    lens getter setter
    where
        getter (ColWrap col) = DataTypeWrap $ _colType col
        setter (ColWrap col) (DataTypeWrap dType) =
            ColWrap $ col {_colType = dType}

colWrapConstraints :: Lens' (ColWrap a) [ColConstraint a]
colWrapConstraints =
    lens getter setter
    where
        getter (ColWrap col) = _colConstraints col
        setter (ColWrap col) name = ColWrap $ col {_colConstraints = name}

-- | SQL data types. Used to define the type of a column.
data DataType b a where
    
    Bool :: DataType Bool a
     
    -- Types related to time.
    Date :: DataType a Time
   
    -- Textual types.
    Char    :: Int -> DataType String a
    Varchar :: Int -> DataType String a
    
    -- Numeric types.
    
    -- | 2 bytes integer – range from -32768 to +32767.
    SmallInt :: DataType Int a 
    
    -- | 4 bytes integer – range from -2147483648 to +2147483647.
    Integer  :: DataType Int a -- ^ 
    
    -- | 8 bytes integer
    --   – range from -9223372036854775808 to +9223372036854775807.
    BigInt   :: DataType Int a 
    
    -- Undefined type.                               
    Undef :: DataType Undefined a
 
-- | Data type wrapper "hidding" the "b" type.                                  
data DataTypeWrap a where
    DataTypeWrap :: DataType b a -> DataTypeWrap a

-- | Constraint on a column.
data ColConstraint a = ColConstraint
    {
      -- | Optional: name of the constraint.
      _colConstraintName :: Maybe String
      
      -- | Type of the constraint.
    , _colConstraintType :: ColConstraintType a
    }

-- | Column constraints types.
data ColConstraintType a where
    -- TODO: | Collate
    
    -- | CHECK.
    Check :: Expression Bool a -> ColConstraintType a
    
    -- | DEFAULT (value).
    Default :: Expression b a -> ColConstraintType a
    
    -- | NOT NULL.
    NotNull :: ColConstraintType a
    
    -- | NULL.
    Null :: ColConstraintType a
    
    -- ^ Primary key. 
    Primary ::
           -- | If 'True', the primary key will have an AUTOINCREMENT.
           Bool  
        -> ColConstraintType a 
    
    -- ^ Foreign key.     
    Reference :: ForeignKey a -> ColConstraintType a
    
    -- | Unique.
    Unique :: ColConstraintType a

--------------------
-- CREATE
--------------------

-- | CREATE statement.
data Create a =
      
      -- | CREATE TABLE statement.
      CreateTable
        { 
          -- | If 'True' an IF NOT EXISTS clause is added.
          _createTableIfNotExists :: Bool
          
          -- | Table to create.
        , _createT :: Table a
        }
        
      -- | CREATE VIEW statement.
    | CreateView
        { 
          -- | If true an IF NOT EXISTS clause is added.
          _createViewIfNotExists :: Bool
          
          -- | View to create.
        , _createV :: View a
        }  

--------------------
-- DROP
--------------------

-- | DROP statement.
data Drop a =

      -- | DROP TABLE statement.
      DropTable
        { 
          -- | If true, an IF EXISTS clause is added.
          _dropTIfExists :: Bool
          
          -- | Table to delete.
        , _dropT :: Table a
        }
      -- | DROP VIEW statement.
    | DropView
        {
          -- | If true, an IF EXISTS clause is added.
          _dropVIfExists :: Bool 
        
        , _dropV :: View a
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
data TableRef a =

      -- | Reference to a table.
      TableRef (Table a) (Maybe (TableRefAs a))

      -- | Reference to a join.
    | JoinRef (Join a) (Maybe (TableRefAs a))

      -- | Reference to a lateral join.
    | LateralRef (SelectWrap a) (TableRefAs a)
    
      -- | Reference to a SELECT.
    | SelectRef (SelectWrap a) (TableRefAs a)   
      
{-|
Return the alias of a table reference if there is one.
Otherwise return Nothing.
-}
getTableRefAlias :: TableRef a -> Maybe (TableRefAs a)
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
getTableRefName :: TableRef a -> String
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
data TableRefAs a = TableRefAs
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
data ColDef b a = ColDef
    { 
      -- | Column.
      _colExpr :: (Column b a)
    
      -- | Table which will be used for the qualified column name.
    , _colExprTableLabel :: Maybe (TableRef a)
    }

-- | Column definition wrapper "hidding" the type "b".
data ColDefWrap a where ColDefWrap :: ColDef b a -> ColDefWrap a

{-|
Generic definition of a column reference used in SELECT queries or in the WHERE
clause of UPDATE statements.
-}
data ColRef b a = ColRef
    {
      -- | Expression of the column reference
      --   (which is not necesserely a column).
      _colRefExpr  :: Expression b a
      
      -- | Label used to reference the column reference (AS).
    , _colRefLabel :: Maybe String
    }

-- | Column reference wrapper "hidding" the type "b".
data ColRefWrap a where ColRefWrap :: ColRef b a -> ColRefWrap a

-- | Values which can be used in data manipulation statements.
data Value b a where

    -- | Boolean value.
    BoolVal :: Bool -> Value Bool a
    
    -- | Numeric value.
    NumericVal :: (Show b, Num b) => b -> Value Numeric a
    
    -- | Float value.
    FloatVal :: Float -> Value Float a
    
    -- | Double value.
    DoubleVal :: Double -> Value Double a
    
    -- | Integer value.
    IntVal :: Int -> Value Int a
    
    -- | String value.
    StringVal :: String -> Value String a
    
    -- | Value of generic type which does not need to be quoted.
    GenVal :: (Raw c, Show c) => c -> Value b a
    
    -- | Value of generic type which needs must quoted.
    GenQVal :: String -> Value b a
    
    -- | Default value (for INSERT, UPDATE or CREATE TABLE statements).
    DefaultVal  :: Value b a
    
    -- Null values.
    
    -- | NULL value of generic type.
    NullVal :: Value b a
    
    -- Placeholders
    
    -- | Placeholder for a value of generic type.
    Placeholder :: Value b a
    
    -- | Placeholder for a boolean value.
    PlaceBool :: Value Bool a
    
    -- | Placeholder for a numeric value.
    PlaceNum :: Value Numeric a
    
    -- | Placeholder for a floating number value.
    PlaceFloat :: Value Float a

    -- | Placeholder for a double precision number value.
    PlaceDouble :: Value Double a
    
    -- | Placeholder for a integer value.
    PlaceInt :: Value Int a
    
    -- | Placeholder for a string value.
    PlaceString :: Value String a

-- | Value wrapper to hide the type "b".
data ValueWrap a where
    ValueWrap :: Value b a -> ValueWrap a

--------------------
-- SELECT
--------------------

-- | SELECT statement.
data Select b a =
      
      -- | SELECT query.
      Single (SelectQ b a)
      
      -- | Combined query such as UNION.
    | Combined (Combination a) [Select b a]

setSelect ::
       
       -- | Lens.
       ASetter (SelectQ b a) (SelectQ b a) d c 
    
       -- | Value to set.
    -> c                                 
    
       -- | Original SELECT query.
    -> Select b a
       
       -- | SELECT query with the newly set value.                         
    -> Select b a                              
setSelect l val query = 
    case query of
        Single sq -> Single $ set l val sq
        Combined combi sqs -> Combined combi $ map (setSelect l val) sqs

-- | The different possible combinations of SELECT queries.
data Combination a =
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
data SelectQ b a = SelectQ
    {
      -- | Type of the SELECT (ALL or DISTINCT).
      _selectType :: SelectType a
    
      -- | Selected columns
    , _selectCols :: Selection b a
    
      -- | FROM clause.
    , _selectFrom :: Maybe (From a)
    
      -- | WHERE clause.
    , _selectWhere :: Maybe (Where a)
    
      -- | GROUP BY clause.
    , _selectGroupBy :: Maybe (GroupBy a)
    
      -- | ORDER BY clause.
    , _selectOrderBy :: Maybe (OrderBy a)
    }

-- | SELECT statement wrapper.
data SelectWrap a where
    SelectWrap :: Select b a -> SelectWrap a

-- Selection clause
--------------------

-- | Type of SELECT query (ALL or DISTINCT).
data SelectType a =
     All
   | Distinct
   | DistinctOn [ColRefWrap a]

-- | Columns selected by a SELECT query.
data Selection b a where

    -- | A single column.
    --   The returned type is a list representation of the type of that column.
    --   For example, if the column is a Numeric type
    --   then the returned type "b" is [Numeric].
    TSelection :: ColRef b a -> Selection b a
    
    -- | Multiple columns of the same type.
    --   The returned type is a list of list representation of the type of
    --   the columns.
    --   For example, if the columns are of type Numeric the return type "b"
    --   will be [[Numeric]].
    TsSelection :: [ColRef b a] -> Selection b a
    
    -- | A single column of undefined type.
    --   The returned type is [Undefined].
    USelection  :: ColRefWrap a -> Selection [Undefined] a
    
    -- | Multiple columns of different or undefined types.
    --   The return type is [[Undefined]].
    UsSelection :: [ColRefWrap a] -> Selection [[Undefined]] a
    
    -- | Single column containing an aggregate value (see 'Aggregate' to get
    --   more information).
    --   The returned type is Numeric.
    ASelection :: ColRef Aggregate a -> Selection Numeric a
    
    -- | Multiple columns containing aggregate values (see 'Aggregate' to get
    --   more information).
    --   The returned type is [Numeric].
    AsSelection :: [ColRef Aggregate a] -> Selection [Numeric] a
    
    -- | Single column containing an aggregate predicate
    --   (see 'AggrPred' to get more information).
    --   The returned type is Bool.
    APSelection :: ColRef AggrPred a -> Selection Bool a
    
    -- | Multiple columns containing aggregate predicates
    --   (see 'AggrPred' to get more information).
    --   The returned type is [Bool].
    APsSelection :: [ColRef AggrPred a] -> Selection [Bool] a

-- | Selection wrapper.
data SelectionWrap a where
    SelectionWrap :: Selection b a -> SelectionWrap a

-- | Return the selected columns of a Selection.
getSelectedCols :: Selection b a -> [ColRefWrap a]
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
data From a = From {_fromTableRefs :: [TableRef a]}

-- | A JOIN between two tables. 
data Join a =
    
      -- | A join which takes only tables references as parameter
      --   – CROSS and NATURAL joins.
      JoinTable
        { _joinTableType   :: JoinTypeTable a
        , _joinTableTable1 :: TableRef a
        , _joinTableTable2 :: TableRef a
        }
      
      -- | A join which has an ON or USING clause.
    | JoinCol
        { _joinColType   :: JoinTypeCol a
        , _joinColTable1 :: TableRef a
        , _joinColTable2 :: TableRef a
        , _joinColClause :: JoinClause a
        }

-- | JOIN clause.
data JoinClause a =
      
      -- | ON clause.
      JoinClauseOn (Expression Bool a)
      
      -- | USING clause.
    | JoinClauseUsing [ColWrap a]

-- | JOIN between two columns references.
data JoinTypeCol a =
      FullJoin 
    | LeftJoin
    | InnerJoin
    | RightJoin

-- | JOIN between two tables references.
data JoinTypeTable a =
      CrossJoin
    | NaturalInnerJoin
    | NaturalLeftJoin
    | NaturalRightJoin
    | NaturalFullJoin

-- WHERE clause
--------------------

-- | WHERE clause.
data Where a = Where
    { 
      -- | Predicate.
      _whereExpr :: Expression Bool a
    }

-- GROUP BY and HAVING clauses
--------------------

{-|
GROUP BY clause.

Note: the GROUP BY clause holds an optional HAVING clause. The structure is
as such because it is not possible to use a HAVING clause without a GROUP BY
one.
-}
data GroupBy a = GroupBy
    { 
      -- | Columns references of the GROUP BY clause.
      _groupByColRefs :: [ColRefWrap a]
      
      -- | HAVING clause.
    , _groupByHaving :: Maybe (Having a)
    }

-- | HAVING clause.
data Having a =
    
      -- | Predicate.
      HavingPred (Expression Bool a)
      
      -- | Aggregated predicate (see 'AggrPred' to get more information).
    | HavingAggrPred (Expression AggrPred a)

-- | Return the expression of a HAVING clause in an expression wrapper.
getHavingExpr :: Having a -> ExprWrap a
getHavingExpr (HavingPred expr)     = ExprWrap expr
getHavingExpr (HavingAggrPred expr) = ExprWrap expr

{- |
ORDER BY query part.

LIMIT and OFFSET can also be specified on top of the ORDER BY.
Those are specified here - and not as query parts - because defining a limit or
an offset without any order by clause would result in inconsistant results.
Indeed, SQL does not guarantee any specific return order if not mentioned.
-}
data OrderBy a = OrderBy
    {
      -- | Definition of the sorting order of the columns.
      _orderByCols :: [SortRef a]
      
      -- | Optional OFFSET clause.
    , _orderByOffset :: Maybe (Offset a)
      
      -- | Optional LIMIT clause.
    , _orderByLimit :: Maybe (Limit a)
    }

-- | Defines how a given column is sorted.
data SortRef a = SortRef
    {
      -- | Sorted column reference.
      --   A wrapper is used since the type of the column does not matter here.
      --   SQL is able to sort any type, even NULL values.
      _sortRefColRef :: ColRefWrap a         
    
      -- | Sorting order (ASC or DESC).
    , _sortRefOrder :: Maybe (SortOrder a)  
    
      -- | NULL values in first or last position.
    , _sortRefNulls :: Maybe (SortNulls a)  
    }

-- | Sorting order (ASC or DESC).
data SortOrder a =
      Asc
    | Desc

-- | NULLS FIRST and NULLS LAST parameters for sorting the NULL values.
data SortNulls a =
      NullsFirst
    | NullsLast

-- | OFFSET clause.
data Offset a = Offset {_offsetVal :: Int}
    
-- | LIMIT clause.
data Limit a = Limit {_limitVal :: Int}

--------------------
-- INSERT
--------------------

{-|
A value assigned to a column.
Both must be of the same type (the "b" phantom types must be the same).

Used in UPDATE or INSERT statements.
-}
data Assignment a where
    Assignment ::
        { 
          -- | Column to which the value is assigned.
          _assignCol :: Column b a
          
          -- | Assigned value.
        , _assignExpr :: Expression b a
        } -> Assignment a

getAssignCol :: Assignment a -> ColWrap a
getAssignCol (Assignment col _) = ColWrap col

getAssignExpr :: Assignment a -> ExprWrap a
getAssignExpr (Assignment _ expr) = ExprWrap expr

-- | INSERT statement.
data Insert a = Insert
    {
      -- | Table in which to insert the values.
      _insertTable :: Table a
    
      -- | Column / value assignments for the insertion.
    , _insertAssign :: [Assignment a]
    }

--------------------
-- UPDATE
--------------------

-- | UPDATE statement.
data Update a = Update
    {
      -- | Table to update.
      _updateTable :: Table a
      
      -- | Values to update in the specified columns.
    , _updateAssignments :: [Assignment a]
    
      -- | WHERE clause.
    , _updateWhere :: Maybe (Where a)
    }

--------------------
-- DELETE
--------------------

-- | DELETE statement.
data Delete a = Delete
    {
      -- | Table to delete.
      _deleteTable :: Table a
      
      -- | Optional WHERE clause.
    , _deleteWhere :: Maybe (Where a)
    }

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