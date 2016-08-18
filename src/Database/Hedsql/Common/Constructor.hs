{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Database/Hedsql/Common/Constructor.hs
Description : Constructor functions for SQL statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for SQL statements.

They provide a flexible and natural way to create the SQL AST.


=Building a query

The idea is to provide a limited set of functions with similar or close
naming to SQL.
The results of those functions can then be composed to form complete
statements.
For example, when using the 'select' function, you will not provide a FROM
clause but only the arguments specific to the 'select' clause which are the
columns.

@
mySelect :: Select [[Undefined]] a

mySelect =
    select [firstName, age]
    where
        firstName = col "firstName" $ varchar 256
        age = col "age" integer
@

The additional FROM clause can be added using do notation.
The monad in which the query is stored is a 'State' monad.
As we are now using a monad, the returned type of the SELECT query will change.
In our case, it will be 'Query' instead of 'Select'.
Note that in our previous example we could also have used a monad and thus the
same 'Query' type.

Adding a FROM clause to our previous example leads to:

@
mySelect :: Query [[Undefined]] a
mySelect = do
    select [firstName, age]
    from people
    where
        firstName = col "firstName" $ varchar 256
        age = col "age" $ integer
        people = table "People"
@

Thanks to type classes, those functions are polymorphic.
It is therefore possible to pass different type of argument to the same
functions. Let's take a look at the 'select' function:

> select $ col "col1" $ varchar 256
> select [col "col1" $ varchar 256, col "col2" varchar 256]

Both above examples are valid.
We can first see that it's possible to pass a single argument or a list to
the 'select' function.
It would also be possible to pass arguments of type 'String' using the extension
('Ext') module.

==Type signature
The signatures of the returned types are composed of one to two phantom types.
The first one is the type of the element in SQL. The second one is the SQL
vendor. For example, the following type:

> Select [Int] SqLite

means that it is a SELECT query returning one column of integers and is a
SQLite statement. Such statement could therefore be parsed only by the SqLite
parser.

If your statement can work for all vendors, you can you use a generic type:

> Select [Int] a

Such statement could then be parsed for any vendor.

=Naming

Most functions have the same name as their SQL functions counterpart.
However, since some words are reserved in Haskell, an underscore is added at
the end in some cases (as does Esqueletto):
- WHERE becomes 'where_'
- AND becomes 'and_'
- AS becomes 'as_'
- IN becomes 'in_'
- OR becomes 'or_'
-}
module Database.Hedsql.Common.Constructor
    (
      -- * Table and table reference

      {-|
      A 'Table' represents a database table which can be used in CREATE or DROP
      statements.

      A 'TableRef' on the other hand is a reference to a table. It could be
      a table, but it could also be a 'Join'.
      It can be used in data manipulation statements (SELECT, INSERT, UPDATE or
      DELETE).

      In practice a 'Table' can also be used in data manipulation statements:
      in such case it will be coerced automatically to a table reference by the
      related function itself.
      The reverse is not true: you cannot use a 'TableRef' in a CREATE or DROP
      statement. If you want to do this, you can use the 'Database.Hedsql.Ext'
      module. It comes of course at the cost of lesser type safety.
      -}
      ToTable
    , table
    , ToTableRef
    , alias
    , tableRef

      -- * Column and column reference
    , ToCol
    , col
    , toCol
    , ToColRef
    , colRef
    , colRefWrap
    , (/.)
    , as_

    -- * Expression
    , expr
    , exprs

      -- * Types

      -- ** Character types
    , char
    , varchar

      -- ** Numeric types
    , bigInt
    , integer
    , smallInt

      -- ** Other types
    , boolean
    , date

    , assign

      -- * Wrapper
    , wrap

      -- * Composition
    , (/++)
    , (|>)
    , end

      {-|
      Type synonym for a 'State' monad. It allows to have simpler type signature
      when creating 'Select' queries.
      -}
    , CreateStmt

      -- * CREATE
    , createTable
    , createTableIfNotExist
    , check
    , checkT
    , colConstraint
    , defaultValue
    , foreignKey
    , notNull
    , nullable
    , primary
    , primaryT
    , constraints
    , unique
    , uniqueT
    , createView
    , createViewIfNotExist

    -- ** DROP
    , dropTable
    , dropTableIfExists
    , dropView
    , dropViewIfExists

      -- * SELECT

      -- ** Selection clause
    , SelectConstr
    , select
    , selectDistinct
    , simpleSelect
    , isDistinctFrom
    , isNotDistinctFrom
    , SelectionConstr
    , selection
    , (//*)

      -- ** FROM clause
    , from
    , ToJoinClause
    , joinClause
    , crossJoin
    , fullJoin
    , innerJoin
    , leftJoin
    , rightJoin
    , naturalFullJoin
    , naturalInnerJoin
    , naturalLeftJoin
    , naturalRightJoin
    , subQuery

    -- ** WHERE clause
    , where_

    -- ** GROUP BY clause
    , ToSortRef
    , groupBy
    , having

    -- ** ORDER BY clause
    , orderBy
    , asc
    , desc
    , sortRef
    , nullsFirst
    , nullsLast

    -- ** LIMIT clause
    , offset
    , limit

    -- ** Combined queries
    , combinedQuery
    , except
    , exceptAll
    , intersect
    , intersectAll
    , union
    , unionAll

    -- * INSERT
    , insert

    -- * UPDATE
    , update

    -- * DELETE
    , deleteFrom

    -- * Values
    , ToSqlValue
    , value
    , boolVal
    , numVal
    , intVal
    , stringVal

    -- ** Placeholders

    {-|
    Placeholders (?) to be bused in queries rather than direct values (which
    is the most advisable SQL technique).
    -}
    , pBool
    , pNum
    , pFloat
    , pDouble
    , pInt
    , pString

      -- * Functions

      -- ** Operators
    , (/+)
    , (/-)
    , (/*)
    , (/==)
    , (/>)
    , (/>=)
    , (/<)
    , (/<=)
    , (/<>)

      -- ** Comparison
    , between
    , exists
    , in_
    , Database.Hedsql.Common.Constructor.like
    , notBetween
    , notIn

      -- ** Logic
    , and_
    , or_

    -- ** Conditions
    , isFalse
    , isNotFalse
    , isNotNull
    , isNotTrue
    , isNotUnknown
    , isNull
    , isTrue
    , isUnknown

    -- ** Maths
    , count
    , max_
    , min_
    , random
    , sum_

    -- ** Utils
    , lastInsertId

    -- ** Dates
    , currentDate

    -- * Statement

    {-|
    Ultimately any SQL instruction is a 'Statement' (SELECT, INSERT, etc.).
    In Hedsql only a 'Statement' can be parsed.
    However, to do that, you don't not need to use these functions, since the
    'parse' function takes care to convert any top expression (SELECT, INSERT,
    etc.) to a 'Statement'.
    Therefore, it's only if you whish to perform your own operations on
    'Statement' that you'll need to use them.
    -}
    , ToStmt
    , statement

    -- * Utility functions
    , ToList
    , toList
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Unsafe.Coerce

import Control.Lens hiding (assign, from, (|>))

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Grammar

--------------------------------------------------------------------------------
-- Table
--------------------------------------------------------------------------------

-- | Convert a value to a 'Table' which can then be used as so in a query.
class ToTable a b | a -> b where
    table :: a -> b

-- | Create a table from itself.
instance ToTable (Table dbVendor) (Table dbVendor) where
    table = id

-- | Create a table from its name provided as 'String'
instance ToTable (SqlString dbVendor) (Table dbVendor) where
    table name = Table name [] []

-- | Create a table reference alias using AS.
alias :: ToTableRef a (TableRef dbVendor) => a -> String -> TableRef dbVendor
alias t name =
    setAlias ref
    where
        al  = TableRefAs name []
        ref = tableRef t
        setAlias (LateralRef a _) = LateralRef a al
        setAlias (SelectRef  a _) = SelectRef  a al
        setAlias (JoinRef    a _) = JoinRef    a (Just al)
        setAlias (TableRef   a _) = TableRef   a (Just al)

{-|
Convert a value to a table reference ('TableRef')
which can then be used in a query.
-}
class ToTableRef a b | a -> b where
    tableRef :: a -> b

instance ToTableRef (Join dbVendor) (TableRef dbVendor) where
    tableRef j = JoinRef j Nothing

instance ToTableRef (Table dbVendor) (TableRef dbVendor) where
    tableRef name = TableRef name Nothing

instance ToTableRef (TableRef dbVendor) (TableRef dbVendor) where
    tableRef = id

--------------------------------------------------------------------------------
-- Column and column reference
--------------------------------------------------------------------------------

-- | Coerce a given type to a list of 'Column'.
class ToCol a b | a -> b where
    toCol :: a -> b

instance ToCol (Column colType dbVendor) (Column colType dbVendor) where
    toCol = id

-- | Create one column which can then be used in a query or a statement.
col ::
       String                    -- ^ Name of the column.
    -> DataType colType dbVendor -- ^ Data type of the column.
    -> Column   colType dbVendor
col name d = Column name d []

{-|
Coerce a given type to a list of 'ColRef', a column reference which can be used
in SELECT clause.
-}
class ToColRef a b | a -> b where
    colRef :: a -> b

instance ToColRef (ColRef colType dbVendor) (ColRef colType dbVendor) where
    colRef = id

instance ToColRef (ColRefWrap dbVendor) (ColRef Undefined dbVendor) where
    -- The unsafeCoerce allows to have a ColRef of undefined type returned.
    colRef (ColRefWrap ref) = unsafeCoerce ref

instance ToColRef (Column colType dbVendor) (ColRef colType dbVendor) where
    colRef a = ColRef (ColExpr $ ColDef a Nothing) Nothing

instance ToColRef (Expression colType dbVendor) (ColRef colType dbVendor) where
    colRef e = ColRef e Nothing

instance ToColRef (Value colType dbVendor) (ColRef colType dbVendor) where
    colRef val = ColRef (Value val) Nothing

instance ToColRef [Value colType dbVendor] (ColRef [colType] dbVendor) where
    colRef xs = ColRef (Values $ map value xs) Nothing

instance ToColRef (Select colType dbVendor) (ColRef colType dbVendor) where
    colRef query = ColRef (SelectExpr query) Nothing

-- | Create a column reference with a qualified name.
(/.) ::
    (  ToTableRef a (TableRef dbVendor)
    ,  ToColRef   b (ColRef colType dbVendor)
    )
    => a -- ^ Name/reference of the table.
    -> b -- ^ Name/reference of the column.
    -> ColRef colType dbVendor
(/.) tName cName =
    case cRef^.colRefExpr of
        ColExpr colDef ->
            set colRefExpr (ColExpr cDef) cRef
            where
                cDef = set colExprTableLabel (Just $ tableRef tName) colDef
        _           ->
            cRef
    where
        cRef = colRef cName

-- | Create a column reference label using AS.
as_ ::
       ToColRef a (ColRef colType dbVendor)
    => a      -- ^ Name/reference of the column.
    -> String -- ^ Output name.
    -> ColRef colType dbVendor
as_ cRef name = set colRefLabel (Just name) (colRef cRef)

{-|
Coerce a type to a 'ColRefWrap'. This is used to wrap the columns
of different types into one single type. Then such standardized type can
be used in lists.
-}
colRefWrap :: ToColRef a (ColRef colType dbVendor) => a -> ColRefWrap dbVendor
colRefWrap = wrap . colRef

{-|
Create a SQL expression which can then be used in condition or column reference.
-}
expr :: ToColRef a (ColRef colType dbVendor) => a -> Expression colType dbVendor
expr = view colRefExpr . colRef

{-|
Create SQL expressions which can then be used in condition or column references.
-}
exprs ::
       ToColRef a (ColRef colType dbVendor)
    => [a]
    -> [Expression colType dbVendor]
exprs = map expr

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Create a BOOLEAN.
boolean :: DataType Bool dbVendor
boolean = Bool

-- | Create a BIGINT.
bigInt :: DataType Int dbVendor
bigInt = BigInt

-- | Create a CHAR.
char :: Int -> DataType String dbVendor
char = Char

-- | Create a DATE.
date :: DataType a Time
date = Date

-- | Create an INTEGER.
integer :: DataType Int dbVendor
integer = Integer

-- | Create a SMALLINT.
smallInt :: DataType Int dbVendor
smallInt = SmallInt

-- | Create a VARCHAR.
varchar :: Int -> DataType String dbVendor
varchar = Varchar

-- | Create a column/value pair to be used in an INSERT or UPDATE statement.
assign ::
    (  ToCol   a (Column colType dbVendor)
    ,  ToColRef b (ColRef colType dbVendor)
    )
    => a -- ^ Column or name of the column.
    -> b -- ^ Value for this column. It can also be an expression.
    -> Assignment dbVendor
assign a val = Assignment (toCol a) (expr val)

--------------------------------------------------------------------------------
-- Wrapper
--------------------------------------------------------------------------------

{-|
Wrap a type "M a b" into a type "M a" using its wrapper.
For example:
> myWrap :: Column a b -> ColWrap a
> myWrap = wrap

This technique allows to build heterogeneous list of elements in a standardized
way with always the same function call.
-}
class Wrapper a b | a -> b where
    wrap :: a -> b

instance Wrapper (Column colType dbVendor) (ColWrap dbVendor) where
    wrap = ColWrap

instance Wrapper (ColRef colType dbVendor) (ColRefWrap dbVendor) where
    wrap = ColRefWrap

instance Wrapper (Select colType dbVendor) (SelectWrap dbVendor) where
    wrap = SelectWrap

instance Wrapper (Value colType dbVendor) (ValueWrap dbVendor) where
    wrap = ValueWrap

--------------------------------------------------------------------------------
-- Composition
--------------------------------------------------------------------------------

{-|
Compose the different part of the SQL statement.

This function is actually just a reverse function composition:
> flip ($)
-}
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

{-|
Clauses a SQL statement by giving its final type.

Without this class SQL statements with different structures would
have different types. For example:
> SELECT * FROM table1;
would have a different type than:
> SELECT * FROM table1 WHERE id = 1;

The 'end' function can be seen as the semi-column used to terminate
a SQL statement.
-}
class End a b | a -> b where
    end :: a -> b

instance End (CreateTableStmt dbVendor) (Create dbVendor) where
    end (CreateTableStmt e t) = CreateTable e t

instance End (CreateTableConstraintStmt dbVendor) (Create dbVendor) where
    end (CreateTableConstraintStmt c stmt) =
        end stmt & _CreateTable . _2 . tableConsts .~ c

instance End (Select colType dbVendor) (Select colType dbVendor) where
    end = id

instance End (SelectSingleStmt colType dbVendor) (Select colType dbVendor) where
    end (SelectSingleStmt t s) = Single $
        SelectQ
            t
            s
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing

instance End (SelectFromStmt colType dbVendor) (Select colType dbVendor) where
    end (SelectFromStmt f selectFromStmt) =
        end selectFromStmt & _Single . selectFrom .~ Just f

instance End (SelectWhereStmt colType dbVendor) (Select colType dbVendor) where
    end (SelectWhereStmt w selectFromStmt) =
        end selectFromStmt & _Single . selectWhere .~ Just w

instance End
    (SelectGroupByStmt colType dbVendor)
    (Select colType dbVendor)
    where
        end (SelectFromGroupByStmt g stmt) =
            end stmt & _Single . selectGroupBy .~ Just g
        end (SelectWhereGroupByStmt g stmt) =
            end stmt & _Single . selectGroupBy .~ Just g

instance End
    (SelectHavingStmt colType dbVendor)
    (Select colType dbVendor)
    where
        end (SelectHavingStmt h stmt) =
            end stmt & _Single . selectHaving .~ Just h

instance End
    (SelectOrderByStmt colType dbVendor)
    (Select colType dbVendor)
    where
        end (SelectFromOrderByStmt o stmt) =
            end stmt & _Single . selectOrderBy .~ Just o
        end (SelectWhereOrderByStmt o stmt) =
            end stmt & _Single . selectOrderBy .~ Just o
        end (SelectGroupByOrderByStmt o stmt) =
            end stmt & _Single . selectOrderBy .~ Just o
        end (SelectHavingOrderByStmt o stmt) =
            end stmt & _Single . selectOrderBy .~ Just o

instance End (SelectLimitStmt colType dbVendor) (Select colType dbVendor) where
    end (SelectLimitStmt l stmt) = end stmt & _Single . selectLimit .~ Just l


instance End (SelectOffsetStmt colType dbVendor) (Select colType dbVendor) where
    end (SelectOffsetStmt o stmt) = end stmt & _Single . selectOffset .~ Just o

instance End (InsertFromStmt dbVendor) (Insert Void dbVendor) where
    end (InsertFromStmt t a) = Insert t a Nothing

instance End
    (InsertReturningStmt colType dbVendor)
    (Insert colType dbVendor)
    where
        end (InsertReturningStmt r stmt) = end stmt & insertReturning .~ Just r

{-|
For UPDATE statement the possibility to create a statement without a WHERE
clause is missing on purpose for avoiding the risk to UPDATE all records
of a table by accident.
-}
instance End (UpdateWhereStmt dbVendor) (Update Void dbVendor) where
    end (UpdateWhereStmt w (UpdateSetStmt t a)) = Update t a (Just w) Nothing

instance End
    (UpdateReturningStmt colType dbVendor)
    (Update colType dbVendor)
    where
        end (UpdateReturningStmt r stmt) = end stmt & updateReturning .~ Just r

instance End (DeleteFromStmt dbVendor) (Delete Void dbVendor) where
    end (DeleteFromStmt t) = Delete t Nothing Nothing

instance End (DeleteWhereStmt dbVendor) (Delete Void dbVendor) where
    end (DeleteWhereStmt w stmt) = end stmt & deleteWhere .~ Just w

instance End
    (DeleteReturningStmt colType dbVendor) (Delete colType dbVendor) where
        end (DeleteFromReturningStmt r stmt) =
            end stmt & deleteReturning .~ Just r
        end (DeleteWhereReturningStmt r stmt) =
            end stmt & deleteReturning .~ Just r

{-|
Allow to easily add optional elements to data types using the '/++' infix
function.
-}
class Add a b where
    addElem ::
           a dbVendor -- ^ Target.
        -> b dbVendor -- ^ Element to add.
        -> a dbVendor -- ^ Target returned with the added element.

-- | Add one constraint to a column.
instance Add ColWrap ColConstraint where
    addElem (ColWrap target) el = ColWrap $ set colConstraints [el] target

-- | Add constraints to a column.
instance Add ColWrap ColConstraints where
    addElem (ColWrap cols) (ColConstraints cs) =
        ColWrap $ set colConstraints cs cols

-- | Add a column constraint type to a column.
instance Add ColWrap ColConstraintType where
    addElem target el = addElem target $ colConstraint "" el

-- | Add a column constraint types to a column.
instance Add ColWrap ColConstraintTypes where
    addElem target (ColConstraintTypes els) =
        addElem target $ ColConstraints $ map (colConstraint "") els

{-|
Coerce a type to another type which can then be used by an Add instance.

This is a hack to allow the use of lists by instances of the Add class since
such instances can only work on phantom types of kind * -> *.

Thus lists are converted to newtypes and the instances for the other types
are just aliases of the id function.
-}
class ToAddable a b | a -> b where
    toConvertible :: a -> b

instance ToAddable (ColConstraint dbVendor) (ColConstraint dbVendor) where
    toConvertible = id

newtype ColConstraints dbVendor = ColConstraints [ColConstraint dbVendor]

instance ToAddable [ColConstraint dbVendor] (ColConstraints dbVendor) where
    toConvertible = ColConstraints

instance ToAddable
    (ColConstraintType dbVendor) (ColConstraintType dbVendor) where
        toConvertible = id

newtype ColConstraintTypes dbVendor =
    ColConstraintTypes [ColConstraintType dbVendor]

instance ToAddable
    [ColConstraintType dbVendor] (ColConstraintTypes dbVendor) where
        toConvertible = ColConstraintTypes

----------------------------------------
-- Public
----------------------------------------

{-|
Allow to easily add optional elements to data types using the @ /++ @ infix
function.
-}
(/++) :: (Add a d, ToAddable b (d dbVendor)) => a dbVendor -> b -> a dbVendor
(/++) target = addElem target . toConvertible

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- TODO: implement ALTER statements.

-- | Return nothing if the provided string is empty.
maybeString :: String -> Maybe String
maybeString ""   = Nothing
maybeString name = Just name

--------------------------------------------------------------------------------
-- CREATE
--------------------------------------------------------------------------------

-- | Create a CHECK constraint.
check :: Expression Bool dbVendor -> ColConstraintType dbVendor
check = Check

-- | Create a CHECK constraint to be used in a table constraint.
checkT :: Maybe String -> Expression Bool dbVendor -> TableConstraint dbVendor
checkT name e = TableConstraint name (TCCheck e) Nothing

-- | Create a constraint which shall then be applied on a column.
colConstraint :: String -> ColConstraintType dbVendor -> ColConstraint dbVendor
colConstraint name = ColConstraint (maybeString name)

-- | Create a CREATE TABLE statement.
createTable ::
       ToTable a (Table dbVendor)
    => a
    -> [ColWrap dbVendor]
    -> CreateTableStmt dbVendor
createTable t c = CreateTableStmt False (table t & tableCols .~ c)

-- | Create a CREATE TABLE IF NOT EXIST statement.
createTableIfNotExist ::
       ToTable a (Table dbVendor)
    => a
    -> [ColWrap dbVendor]
    -> Create dbVendor
createTableIfNotExist t c = CreateTable True (table t & tableCols .~ c)

-- | Create a CREATE VIEW statement.
createView ::
       View dbVendor   -- ^ Select query from which the view is created.
    -> Create dbVendor
createView = CreateView False

-- | Create a CREATE VIEW IF NOT EXIST statement.
createViewIfNotExist ::
       View dbVendor   -- ^ Select query from which the view is created.
    -> Create dbVendor
createViewIfNotExist = CreateView True

-- | Create a DEFAULT value constraint.
defaultValue ::
       ToColRef a (ColRef colType dbVendor)
    => a
    -> ColConstraintType dbVendor
defaultValue e = Default $ expr e

-- | Create a DROP TABLE statement.
dropTable ::
       (ToTable a (Table dbVendor))
    => a      -- ^ Table to drop.
    -> Drop dbVendor
dropTable = DropTable False . table

dropTableIfExists ::
       ToTable a (Table dbVendor)
    => a      -- ^ Table to drop.
    -> Drop dbVendor
dropTableIfExists = DropTable True . table

-- | Create a DROP VIEW statement.
dropView ::
       View dbVendor -- ^ View to drop.
    -> Drop dbVendor
dropView = DropView False

-- | Create a DROP VIEW IF EXISTS statement.
dropViewIfExists ::
       View dbVendor -- ^ View to drop.
    -> Drop dbVendor
dropViewIfExists = DropView True

-- | Create a FOREIGN KEY constraint.
foreignKey ::
    ( ToList b [c]
    , ToTable a (Table dbVendor)
    , ToCol c (Column colType dbVendor)
    )
    => a -- ^ Table.
    -> b -- ^ Columns.
    -> ColConstraintType dbVendor
foreignKey t c =
    Reference $ ForeignKey (table t) cols Nothing Nothing
    where
        cols = map (ColWrap . toCol) $ toList c

-- | Create a NOT NULL constraint.
notNull :: ColConstraintType dbVendor
notNull = NotNull

-- | Create a NULL constraint.
nullable :: ColConstraintType dbVendor
nullable = Null

-- | Create a PRIMARY KEY constraint.
primary ::
       Bool -- ^ If True, the primary key will be an AUTOINCREMENT.
    -> ColConstraintType dbVendor
primary = Primary

-- | Add a PRIMARY KEY constraint to a CREATE statement.
primaryT ::
       (ToList a [b], ToCol b (Column colType dbVendor))
    => Maybe String -- ^ Optional name of the constraint.
    -> a
    -> TableConstraint dbVendor
primaryT name c =
    TableConstraint
        name
        (TCPrimaryKey $ map (ColWrap . toCol) $ toList c)
        Nothing

-- | Add one or more constraints to a CREATE statement.
constraints ::
       (ToList a [TableConstraint dbVendor])
    => a
    -> CreateTableStmt dbVendor
    -> CreateTableConstraintStmt dbVendor
constraints = CreateTableConstraintStmt . toList

-- | Create an UNIQUE column constraint.
unique :: ColConstraintType dbVendor
unique = Unique

-- | Create an UNIQUE table constraint
uniqueT ::
       Maybe String -- ^ Optional name of the constraint.
    -> [ColWrap dbVendor]
    -> TableConstraint dbVendor
uniqueT name cs = TableConstraint name (TCUnique cs) Nothing

--------------------------------------------------------------------------------
-- SELECT
--------------------------------------------------------------------------------

-- | Convert an element to a list with itself as the only item.
list :: a -> [a]
list a = [a]

-- | Coerce a type to a list of JoinClause type such as ON or USING.
class ToJoinClause a b | a -> b where
    joinClause :: a -> b

-- | Create an ON join clause from a boolean function.
instance ToJoinClause (Expression Bool dbVendor) (JoinClause dbVendor) where
    joinClause = JoinClauseOn

-- | Create an USING join clause from a column.
instance ToJoinClause (Column colType dbVendor) (JoinClause dbVendor) where
    joinClause = JoinClauseUsing . list . ColWrap

{-|
Convert a value to a sorting reference: a reference which can be used in an
ORDER BY clause.
-}
class ToSortRef a b | a -> b where
    sortRef :: a -> b

instance ToSortRef (Column colType dbVendor) (SortRef dbVendor) where
    sortRef c = SortRef (ColRefWrap $ colRef c) Nothing Nothing

instance ToSortRef (ColWrap dbVendor) (SortRef dbVendor) where
    sortRef (ColWrap c) = SortRef (ColRefWrap $ colRef c) Nothing Nothing

instance ToSortRef (ColRef colType dbVendor) (SortRef dbVendor) where
    sortRef ref = SortRef (ColRefWrap ref) Nothing Nothing

instance ToSortRef (ColRefWrap dbVendor) (SortRef dbVendor) where
    sortRef ref = SortRef ref Nothing Nothing

instance ToSortRef (SortRef dbVendor) (SortRef dbVendor) where
    sortRef = id

-- | Create a join on columns with a USING or ON clause.
columnJoin ::
   (
      ToTableRef   a (TableRef dbVendor)
   ,  ToTableRef   b (TableRef dbVendor)
   ,  ToJoinClause c (JoinClause dbVendor)
   )
   => JoinTypeCol dbVendor -- ^ Type of join.
   -> a                    -- ^ Name/reference of the first table.
   -> b                    -- ^ Name/reference of the second table.
   -> c                    -- ^ Join clause
   -> Join dbVendor
columnJoin joinType tableRef1 tableRef2 clause =
    JoinCol
         joinType
        (tableRef tableRef1)
        (tableRef tableRef2)
        (joinClause clause)

-- | Create a join on tables (CROSS or NATURAL join).
tableJoin ::
    (  ToTableRef   a (TableRef dbVendor)
    ,  ToTableRef   b (TableRef dbVendor)
    )
    => JoinTypeTable dbVendor -- ^ Type of join.
    -> a                      -- ^ Name/reference of the first table.
    -> b                      -- ^ Name/reference of the second table.
    -> Join dbVendor
tableJoin joinType tableRef1 tableRef2 =
    JoinTable
        joinType
        (tableRef tableRef1)
        (tableRef tableRef2)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- SELECT
----------------------------------------

-- | Create a Select query with only a column selection clause.
simpleSelect :: Selection colType dbVendor -> SelectSingleStmt colType dbVendor
simpleSelect = SelectSingleStmt All

{-|
Allow the creation of SELECT queries with correct types.

A SELECT query returning only one column will have type:
> Select [b] a

A SELECT query returning many columns will have type:
> Select [[b]] a
-}
class SelectConstr a b | a -> b where
    -- | Create a SELECT query.
    select :: a -> b

instance SelectConstr
    (ColRefWrap dbVendor)
    (SelectSingleStmt [Undefined] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    [ColRefWrap dbVendor]
    (SelectSingleStmt [[Undefined]] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    (Column colType dbVendor)
    (SelectSingleStmt [colType] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    [Column colType dbVendor]
    (SelectSingleStmt [[colType]] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    (ColWrap dbVendor)
    (SelectSingleStmt [Undefined] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    [ColWrap dbVendor]
    (SelectSingleStmt [[Undefined]] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    (ColRef colType dbVendor)
    (SelectSingleStmt [colType] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    [ColRef colType dbVendor]
    (SelectSingleStmt [[colType]] dbVendor)
    where
        select = simpleSelect . selection

instance SelectConstr
    (Expression colType dbVendor)
    (SelectSingleStmt [colType] dbVendor)
    where
        select = simpleSelect . selection

{-|
Create a SELECT DISTINCT query.
-}
selectDistinct ::
       SelectionConstr a (Selection colType dbVendor)
    => a
    -> SelectSingleStmt colType dbVendor
selectDistinct = SelectSingleStmt Distinct . selection

-- | Create a IS DISTINCT FROM operator.
isDistinctFrom ::
    ( ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
isDistinctFrom colRef1 colRef2 =
    IsDistinctFrom (colRef colRef1) (colRef colRef2)

-- | Create a IS NOT DISTINCT FROM operator.
isNotDistinctFrom ::
    ( ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
isNotDistinctFrom colRef1 colRef2 =
    IsNotDistinctFrom (colRef colRef1) (colRef colRef2)

{-|
Allow the creation of output selection expressions with correct type.

A selection with only one column will have type:
> Selection [b] a

A selection with many columns will have type:
> Selection [[b]] a
-}
class SelectionConstr a b | a -> b where
    selection :: a -> b

instance SelectionConstr
    (ColRefWrap dbVendor) (Selection [Undefined] dbVendor) where
        selection = USelection

instance SelectionConstr
    [ColRefWrap dbVendor] (Selection [[Undefined]] dbVendor) where
        selection = UsSelection

instance SelectionConstr
    (Column colType dbVendor) (Selection [colType] dbVendor) where
        selection c = TSelection $ colRef column
            where
                -- Unsafe coercion to the correct phantom types parameter.
                column :: Column [colType] dbVendor
                column = unsafeCoerce c

instance SelectionConstr
    [Column colType dbVendor] (Selection [[colType]] dbVendor) where
        selection cs = TsSelection $ map colRef columns
            where
                -- Unsafe coercion to the correct phantom types parameter.
                columns :: [Column [[colType]] dbVendor]
                columns = unsafeCoerce cs

instance SelectionConstr
    (ColWrap dbVendor) (Selection [Undefined] dbVendor) where
        selection (ColWrap c) = USelection $ ColRefWrap $ colRef c

instance SelectionConstr
    [ColWrap dbVendor] (Selection [[Undefined]] dbVendor) where
        selection = UsSelection . map toColRef
            where
                toColRef (ColWrap c) = ColRefWrap $ colRef c

instance SelectionConstr
    (ColRef colType dbVendor) (Selection [colType] dbVendor) where
        selection c = TSelection cRef
            where
                -- Unsafe coercion to the correct phantom types parameter.
                cRef :: ColRef [colType] dbVendor
                cRef = unsafeCoerce c

instance SelectionConstr
    [ColRef colType dbVendor] (Selection [[colType]] dbVendor) where
        selection cs = TsSelection cRefs
            where
                -- Unsafe coercion to the correct phantom types parameter.
                cRefs :: [ColRef [[colType]] dbVendor]
                cRefs = unsafeCoerce cs

instance SelectionConstr
    (Expression colType dbVendor) (Selection [colType] dbVendor) where
        selection c = TSelection $ colRef cRef
            where
                -- Unsafe coercion to the correct phantom types parameter.
                cRef :: Expression [colType] dbVendor
                cRef = unsafeCoerce c

-- | Create a joker - "*" - character.
(//*) :: Expression [Undefined] dbVendor
(//*) = Joker

----------------------------------------
-- FROM
----------------------------------------

-- | Add a FROM clause to a SELECT query.
from ::
       (ToList a [b], ToTableRef b (TableRef dbVendor))
    => a
    -> SelectSingleStmt colType dbVendor
    -> SelectFromStmt colType dbVendor
from tRef =
    SelectFromStmt fromClause
    where
        fromClause = From $ map tableRef $ toList tRef

-- | Create a CROSS JOIN.
crossJoin ::
    (  ToTableRef a (TableRef dbVendor)
    ,  ToTableRef b (TableRef dbVendor)
    )
    => a
    -> b
    -> Join dbVendor
crossJoin = tableJoin CrossJoin

-- | Create a FULL JOIN.
fullJoin ::
    ( ToTableRef   a (TableRef dbVendor)
    , ToTableRef   b (TableRef dbVendor)
    , ToJoinClause c (JoinClause dbVendor)
    )
    => a      -- ^ First table reference.
    -> b      -- ^ Second table reference.
    -> c      -- ^ Join clause.
    -> Join dbVendor
fullJoin = columnJoin FullJoin

{-|
Create an INNER JOIN.
If the join clause is a condition or a boolean function, it will be an ON
clause.
If the join clause is a column, a string or a list of columns or strings, it
will be an USING clause.
-}
innerJoin ::
    ( ToTableRef   a (TableRef dbVendor)
    , ToTableRef   b (TableRef dbVendor)
    , ToJoinClause c (JoinClause dbVendor)
    )
    => a      -- ^ First table reference.
    -> b      -- ^ Second table reference.
    -> c      -- ^ Join clause.
    -> Join dbVendor
innerJoin = columnJoin InnerJoin

-- | Create a LEFT JOIN.
leftJoin ::
    ( ToTableRef   a (TableRef dbVendor)
    , ToTableRef   b (TableRef dbVendor)
    , ToJoinClause c (JoinClause dbVendor)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> c -- ^ Join clause.
    -> Join dbVendor
leftJoin = columnJoin LeftJoin

-- | Create a NATURAL FULL JOIN.
naturalFullJoin ::
    (  ToTableRef a (TableRef dbVendor)
    ,  ToTableRef b (TableRef dbVendor)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> Join dbVendor
naturalFullJoin = tableJoin NaturalFullJoin

-- | Create a NATURAL LEFT JOIN.
naturalLeftJoin ::
    (  ToTableRef a (TableRef dbVendor)
    ,  ToTableRef b (TableRef dbVendor)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> Join dbVendor
naturalLeftJoin = tableJoin NaturalLeftJoin

-- | Create a NATURAL INNER JOIN.
naturalInnerJoin ::
    (  ToTableRef a (TableRef dbVendor)
    ,  ToTableRef b (TableRef dbVendor)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> Join dbVendor
naturalInnerJoin = tableJoin NaturalInnerJoin

-- | Create a NATURAL RIGHT JOIN.
naturalRightJoin ::
    (  ToTableRef a (TableRef dbVendor)
    ,  ToTableRef b (TableRef dbVendor)
    )
    => a -- ^ First table reference.
    -> b -- ^ Second table reference.
    -> Join dbVendor
naturalRightJoin = tableJoin NaturalRightJoin

-- | Create a RIGHT JOIN.
rightJoin ::
    ( ToTableRef   a (TableRef dbVendor)
    , ToTableRef   b (TableRef dbVendor)
    , ToJoinClause c (JoinClause dbVendor)
    )
    => a      -- ^ First table reference.
    -> b      -- ^ Second table reference.
    -> c      -- ^ Join clause.
    -> Join dbVendor
rightJoin = columnJoin RightJoin

-- | Create a sub-query in a FROM clause.
subQuery ::
       (End a (Select b dbVendor))
    => a                 -- ^ Sub-query.
    -> String            -- ^ Alias of the sub-query.
    -> TableRef dbVendor -- ^ Table reference.
subQuery sub name = SelectRef (SelectWrap $ end sub) $ TableRefAs name []

----------------------------------------
-- WHERE
----------------------------------------

class WhereConstr a b | a -> b where
    where_ :: Expression Bool dbVendor -> a dbVendor -> b dbVendor

-- | Create a WHERE clause for a SELECT statement.
instance WhereConstr (SelectFromStmt colType) (SelectWhereStmt colType) where
    where_ = SelectWhereStmt . Where

-- | Create a WHERE clause for an UPDATE statement.
instance WhereConstr UpdateSetStmt UpdateWhereStmt where
    where_ = UpdateWhereStmt . Where

-- | Create a WHERE clause for a DELETE statement.
instance WhereConstr DeleteFromStmt DeleteWhereStmt where
    where_ = DeleteWhereStmt . Where

----------------------------------------
-- ORDER BY
----------------------------------------

-- | Create an ORDER BY clause.
orderByClause ::
    (  ToList a [b]
    ,  ToSortRef b (SortRef dbVendor)
    )
    => a
    -> OrderBy dbVendor
orderByClause = OrderBy . map sortRef . toList

-- | Add an ORDER BY clause to a query.
class OrderByConstr a where
    orderBy ::
        (  ToList s [t]
        ,  ToSortRef t (SortRef dbVendor)
        )
        => s -- ^ Sorting references.
        -> a colType dbVendor
        -> SelectOrderByStmt colType dbVendor

instance OrderByConstr SelectFromStmt where
    orderBy = SelectFromOrderByStmt . orderByClause

instance OrderByConstr SelectWhereStmt where
    orderBy = SelectWhereOrderByStmt . orderByClause

instance OrderByConstr SelectGroupByStmt where
    orderBy = SelectGroupByOrderByStmt . orderByClause

instance OrderByConstr SelectHavingStmt where
    orderBy = SelectHavingOrderByStmt . orderByClause

{-|
Add an ascending sorting order (ASC) to a sort reference
(which can be a column reference).
-}
asc :: ToSortRef a (SortRef dbVendor) => a -> SortRef dbVendor
asc ref =  set sortRefOrder (Just Asc) (sortRef ref)

{-|
Add a descending sorting order (DESC) to a sort reference
(which can be a column reference).
-}
desc :: ToSortRef a (SortRef dbVendor) => a -> SortRef dbVendor
desc ref =  set sortRefOrder (Just Desc) (sortRef ref)

{-|
Add a nulls first option (NULLS FIRST) to a sort reference
(which can be a column reference).
-}
nullsFirst :: ToSortRef a (SortRef dbVendor) => a -> SortRef dbVendor
nullsFirst sRef = set sortRefNulls (Just NullsFirst) (sortRef sRef)

{-|
Add a nulls last option (NULLS LAST) to a sort reference
(which can be a column reference).
-}
nullsLast:: ToSortRef a (SortRef dbVendor) => a -> SortRef dbVendor
nullsLast sRef = set sortRefNulls (Just NullsLast) (sortRef sRef)

----------------------------------------
-- GROUP BY
----------------------------------------

-- | Create a GROUP BY clause.
groupByClause ::
    ( ToList a [b]
    , ToColRef b (ColRef c dbVendor)
    )
    => a -- ^ Grouping references.
    -> GroupBy dbVendor
groupByClause = GroupBy . map colRefWrap . toList

-- | Add a GROUP BY clause to a SELECT statement.
class GroupByConstr a where
    groupBy ::
        ( ToList g [t]
        , ToColRef t (ColRef c dbVendor)
        )
        => g -- ^ Grouping references.
        -> a colType dbVendor
        -> SelectGroupByStmt colType dbVendor

instance GroupByConstr SelectFromStmt where
    groupBy = SelectFromGroupByStmt . groupByClause

instance GroupByConstr SelectWhereStmt where
    groupBy = SelectWhereGroupByStmt . groupByClause

-- | Add a HAVING clause to a SELECT statement having a GROUP BY clause.
having ::
       HavingCond a
    => a dbVendor
    -> SelectGroupByStmt colType dbVendor
    -> SelectHavingStmt colType dbVendor
having = SelectHavingStmt . havingCond

-- | Create a HAVING condition.
class HavingCond a where
    havingCond :: a dbVendor -> Having dbVendor

{-|
Instance for regular predicates – which could also be used in a WHERE clause.
-}
instance HavingCond (Expression Bool) where
    havingCond = HavingPred

{-|
Instance for predicates containing an aggregate function (COUNT, SUM, etc.)
– which couldn't be used in a WHERE clause.
-}
instance HavingCond (Expression AggrPred) where
    havingCond= HavingAggrPred

----------------------------------------
-- LIMIT
----------------------------------------

-- | Add a LIMIT clause to a SELECT statement having an ORDER BY clause.
limit ::
       Int
    -> SelectOrderByStmt colType dbVendor
    -> SelectLimitStmt colType dbVendor
limit = SelectLimitStmt . Limit

-- | Add an OFFSET clause to a SELECT statement having a LIMIT clause.
offset ::
       Int
    -> SelectLimitStmt colType dbVendor
    -> SelectOffsetStmt colType dbVendor
offset = SelectOffsetStmt . Offset

----------------------------------------
-- Combined queries
----------------------------------------

{-|
Combine two SELECT queries using the provided combination clause
(UNION, EXCEPT, etc.).
-}
combinedQuery ::
       (End a (Select colType dbVendor), End b (Select colType dbVendor))
    => Combination dbVendor
    -> a
    -> b
    -> Select colType dbVendor
combinedQuery cType c1 c2 = Combined cType [end c1, end c2]

-- | Apply an EXCEPT to two queries.
except ::
       (End a (Select colType dbVendor), End b (Select colType dbVendor))
    => a
    -> b
    -> Select colType dbVendor
except = combinedQuery Except

-- | Apply an EXCEPT ALL to two queries.
exceptAll ::
       (End a (Select colType dbVendor), End b (Select colType dbVendor))
    => a
    -> b
    -> Select colType dbVendor
exceptAll = combinedQuery ExceptAll

-- | Apply an INTERSECT to two queries.
intersect ::
       (End a (Select colType dbVendor), End b (Select colType dbVendor))
    => a
    -> b
    -> Select colType dbVendor
intersect = combinedQuery Intersect

-- | Apply an INTERSECT ALL to two queries.
intersectAll ::
       (End a (Select colType dbVendor), End b (Select colType dbVendor))
    => a
    -> b
    -> Select colType dbVendor
intersectAll = combinedQuery IntersectAll

-- | Create an UNION operation between two queries.
union ::
       (End a (Select colType dbVendor), End b (Select colType dbVendor))
    => a
    -> b
    -> Select colType dbVendor
union = combinedQuery Union

-- | Create an UNION ALL operation between two queries.
unionAll ::
       (End a (Select colType dbVendor), End b (Select colType dbVendor))
    => a
    -> b
    -> Select colType dbVendor
unionAll = combinedQuery UnionAll

--------------------------------------------------------------------------------
-- INSERT
--------------------------------------------------------------------------------

{-|
Create an INSERT statement.

The values to insert are a list of list of assignments because you may insert
more than one row in the database.
-}
insert ::
    ( ToTable a (Table dbVendor)
    )
    => a -- ^ Table or name of the table to insert the data into.
    -> [Assignment dbVendor] -- ^ Values to insert.
    -> InsertFromStmt dbVendor
insert = InsertFromStmt . table

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------

-- | Create an UPDATE statement.
update ::
       ToTable a (Table dbVendor)
    => a                     -- ^ Table to update.
    -> [Assignment dbVendor] -- ^ Column/value assignments.
    -> UpdateSetStmt dbVendor
update = UpdateSetStmt . table

--------------------------------------------------------------------------------
-- DELETE
--------------------------------------------------------------------------------

{-|
Create a DELETE FROM clause.
-}
deleteFrom ::
       ToTable a (Table dbVendor)
    => a
    -> DeleteFromStmt dbVendor
deleteFrom = DeleteFromStmt . table

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

-- TODO: add the missing types.

{-|
Types used for the coercion.

Those types allow to implement instances complying with the coverage
condition when using instances using functional dependencies.

More concretely we can define instances such as:
>instance CoerceToX (SqlString a) (X a) where
>    coerceToX = [...]
-}
type SqlBool   dbVendor = Bool
type SqlString dbVendor = String
type SqlInt    dbVendor = Int

{-|
Convert a primitive value so it can be used in SQL queries as values.
-}
class ToSqlValue a b | a -> b where
    value :: a -> b

instance ToSqlValue (Value dbVendor colType) (Value dbVendor colType) where
    value = id

instance ToSqlValue (SqlBool dbVendor) (Value Bool dbVendor) where
    value = BoolVal

instance ToSqlValue (SqlString dbVendor) (Value String dbVendor) where
    value = StringVal

instance ToSqlValue (SqlInt dbVendor) (Value Int dbVendor) where
    value = IntVal

-- | Create a boolean value.
boolVal :: Bool -> Value Bool dbVendor
boolVal = BoolVal

-- | Create a string value.
stringVal :: String -> Value String dbVendor
stringVal = StringVal

-- | Create an integer value.
intVal :: Int -> Value Int dbVendor
intVal = IntVal

-- | Create a numeric value.
numVal :: (Show b, Num b) => b -> Value Numeric dbVendor
numVal = NumericVal

---------------------------------------
-- Placeholder
---------------------------------------

-- | Create a placeholder "?" for a boolean value.
pBool :: Value Bool dbVendor
pBool = PlaceBool

-- | Create a placeholder "?" for a numeric value.
pNum :: Value Numeric dbVendor
pNum = PlaceNum

-- | Create a placeholder "?" for a floating number value.
pFloat :: Value Float dbVendor
pFloat = PlaceFloat

-- | Create a placeholder "?" for a double precision number value.
pDouble :: Value Double dbVendor
pDouble = PlaceDouble

-- | Create a placeholder "?" for a integer value.
pInt :: Value Int dbVendor
pInt = PlaceInt

-- | Create a placeholder ? for a string value.
pString :: Value String dbVendor
pString = PlaceString

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

---------------------------------------
-- Operators
---------------------------------------

-- | "+" operator.
(/+) ::
    ( Num colType
    , ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression colType dbVendor
(/+) left right = Add (colRef left) (colRef right)

-- | "-" operator.
(/-) ::
    ( Num colType
    , ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression colType dbVendor
(/-) left right = Substract (colRef left) (colRef right)

-- | "*" operator.
(/*) ::
    ( Num colType
    , ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression colType dbVendor
(/*) left right = Multiply (colRef left) (colRef right)

-- | Equality operator ("=" in SQL).
infix 7 /==
(/==) ::
    ( ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
(/==) colRef1 colRef2 = Equal (colRef colRef1) (colRef colRef2)

-- | Greater than operator (">").
infix 7 />
(/>) ::
    ( ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
(/>) colRef1 colRef2 = GreaterThan (colRef colRef1) (colRef colRef2)

-- | Greater than or equal to operator (">=").
infix 7 />=
(/>=) ::
    ( ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
(/>=) colRef1 colRef2 = GreaterThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Smaller than operator ("<").
infix 7 /<
(/<) ::
    ( ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
(/<) colRef1 colRef2 = SmallerThan (colRef colRef1) (colRef colRef2)

-- | Smaller than or equal to operator ("<=").
infix 7 /<=
(/<=) ::
    (
      SQLOrd colType
    , ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
(/<=) colRef1 colRef2 = SmallerThanOrEqTo (colRef colRef1) (colRef colRef2)

-- | Unequality operator ("<>").
infix 7 /<>
(/<>) ::
    ( ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
(/<>) colRef1 colRef2 = NotEqual (colRef colRef1) (colRef colRef2)

---------------------------------------
-- Logic
---------------------------------------

-- TODO: find a way to enclose them in parenthesis.
-- The solution is probabyl to:
-- - add a paremeter to the AND constructor. If it is 'True', parenthesis are
--   there.
-- - use a 'parens' function to turn this parameter to 'True'.

-- | Join two predicates with an AND.
and_ ::
       Expression Bool dbVendor
    -> Expression Bool dbVendor
    -> Expression Bool dbVendor
and_ c1 c2 = And c1 c2 False

-- | Join two predicates with an OR.
or_ ::
       Expression Bool dbVendor
    -> Expression Bool dbVendor
    -> Expression Bool dbVendor
or_ c1 c2 = Or c1 c2 False

---------------------------------------
-- Conditions
---------------------------------------

-- | BETWEEN condition.
between ::
    (
      ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    , ToColRef c (ColRef colType dbVendor)
    )
    => a                        -- ^ Expression to evaluate.
    -> b                        -- ^ Lower bound condition.
    -> c                        -- ^ Higher bound condition.
    -> Expression Bool dbVendor -- ^ Between condition.
between ex lower higher = Between (colRef ex) (colRef lower) (colRef higher)

-- | NOT BETWEEN condition.
notBetween ::
    (
      ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef colType dbVendor)
    , ToColRef c (ColRef colType dbVendor)
    )
    => a                        -- ^ Expression to evaluate.
    -> b                        -- ^ Lower bound condition.
    -> c                        -- ^ Higher bound condition.
    -> Expression Bool dbVendor -- ^ Not between condition.
notBetween ex lower higher =
    NotBetween (colRef ex) (colRef lower) (colRef higher)

-- | Create an EXISTS function.
exists :: ToColRef a (ColRef colType dbVendor) => a -> Expression Bool dbVendor
exists = Exists . colRef

-- | Create an IN operator.
in_ ::
    (
      ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef [colType] dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
in_ colRef1 colRef2 = In (colRef colRef1) (colRef colRef2)

-- | Create a NOT IN operator.
notIn ::
    (
      ToColRef a (ColRef colType dbVendor)
    , ToColRef b (ColRef [colType] dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
notIn colRef1 colRef2 = NotIn (colRef colRef1) (colRef colRef2)

-- | Create a IS FALSE function.
isFalse :: ToColRef a (ColRef Bool dbVendor) => a -> Expression Bool dbVendor
isFalse = IsFalse . colRef

-- | Create a IS NOT FALSE function.
isNotFalse :: ToColRef a (ColRef Bool dbVendor) => a -> Expression Bool dbVendor
isNotFalse = IsNotFalse . colRef

-- | Create a IS NOT NULL function.
isNotNull ::
       ToColRef a (ColRef colType dbVendor)
    => a
    -> Expression Bool dbVendor
isNotNull = IsNotNull . colRef

-- | Create a IS NOT TRUE function.
isNotTrue :: ToColRef a (ColRef Bool dbVendor) => a -> Expression Bool dbVendor
isNotTrue = IsNotTrue . colRef

-- | Create a IS NOT UNKNOWN function.
isNotUnknown ::
       ToColRef a (ColRef colType dbVendor)
    => a
    -> Expression Bool dbVendor
isNotUnknown = IsNotUnknown . colRef

-- | Create a IS NULL function.
isNull :: ToColRef a (ColRef colType dbVendor) => a -> Expression Bool dbVendor
isNull = IsNull . colRef

-- | Create a IS TRUE function.
isTrue :: ToColRef a (ColRef Bool dbVendor) => a -> Expression Bool dbVendor
isTrue = IsTrue . colRef

-- | Create a IS UNKNOWN function.
isUnknown ::
       ToColRef a (ColRef colType dbVendor)
    => a
    -> Expression Bool dbVendor
isUnknown = IsUnknown . colRef

-- | Create a LIKE operator.
like ::
    (  ToColRef a (ColRef String dbVendor)
    ,  ToColRef b (ColRef String dbVendor)
    )
    => a
    -> b
    -> Expression Bool dbVendor
like colRef1 colRef2 = Like (colRef colRef1) (colRef colRef2)

---------------------------------------
-- Maths
---------------------------------------

-- | Create a COUNT function.
count :: ToColRef a (ColRef colType dbVendor) => a -> Expression Int dbVendor
count = Count . colRef

-- | Create a MAX function.
max_ ::
      (Num colType, ToColRef a (ColRef colType dbVendor))
    => a
    -> Expression colType dbVendor
max_ = Max . colRef

-- | Create a MIN function.
min_ ::
      (Num colType, ToColRef a (ColRef colType dbVendor))
    => a
    -> Expression colType dbVendor
min_ = Min . colRef

-- | Create a random() function.
random :: Num colType => Expression colType dbVendor
random = Random

-- | Create a SUM function.
sum_ ::
      (Num colType, ToColRef a (ColRef colType dbVendor))
    => a
    -> Expression colType dbVendor
sum_ = Sum . colRef

---------------------------------------
-- Dates
---------------------------------------

{-|
Create a function which will return the current date.
-}
currentDate :: Expression Time dbVendor
currentDate = CurrentDate

---------------------------------------
-- Utils
---------------------------------------

{-|
Create a function which will return the primary key value of the last inserted
row.

This function differs from one database vendor from another.
Note that with PostgreSQL you may prefer to use the RETURNING clause instead.
-}
lastInsertId :: Expression colType dbVendor
lastInsertId = LastInsertId

--------------------------------------------------------------------------------
-- Statement
--------------------------------------------------------------------------------

-- | Convert a value to a 'Statement'.
class ToStmt a b | a -> b where
    statement :: a -> b

instance ToStmt [Statement dbVendor] (Statement dbVendor) where
    statement = Statements

instance ToStmt (Create dbVendor) (Statement dbVendor) where
    statement = CreateStmt

instance ToStmt (CreateTableStmt dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (CreateTableConstraintStmt dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (Delete colType dbVendor) (Statement dbVendor) where
    statement = DeleteStmt . DeleteWrap

instance ToStmt (DeleteFromStmt dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (DeleteWhereStmt dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt
    (DeleteReturningStmt colType dbVendor)
    (Statement dbVendor)
    where
        statement = statement . end

instance ToStmt (Drop dbVendor) (Statement dbVendor) where
    statement = DropStmt

instance ToStmt (Insert colType dbVendor) (Statement dbVendor) where
    statement = InsertStmt . InsertWrap

instance ToStmt (InsertFromStmt dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt
    (InsertReturningStmt colType dbVendor)
    (Statement dbVendor)
    where
        statement = statement . end

instance ToStmt (Select colType dbVendor) (Statement dbVendor) where
    statement = SelectStmt . SelectWrap

instance ToStmt (SelectSingleStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectFromStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectWhereStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectGroupByStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectHavingStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectOrderByStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectLimitStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectOffsetStmt colType dbVendor) (Statement dbVendor) where
    statement = statement . end

instance ToStmt (SelectWrap dbVendor) (Statement dbVendor) where
    statement = SelectStmt

instance ToStmt (Update colType dbVendor) (Statement dbVendor) where
    statement = UpdateStmt . UpdateWrap

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

{-|
Convert an element to a list or, if it is already a list return the list as is
('id' function).

This class and its instances allow to pass a list or a single element to a
function. This mimic SQL with the SELECT or FROM clauses for example which
can take one or more argument.
-}
class ToList a b | a -> b where
    toList :: a -> b

instance ToList (Table dbVendor) [Table dbVendor] where
    toList x = [x]

instance ToList [Table dbVendor] [Table dbVendor] where
    toList = id

instance ToList (TableRef dbVendor) [TableRef dbVendor] where
    toList x = [x]

instance ToList [TableRef dbVendor] [TableRef dbVendor] where
    toList = id

instance ToList (TableConstraint dbVendor) [TableConstraint dbVendor] where
    toList x = [x]

instance ToList [TableConstraint dbVendor] [TableConstraint dbVendor] where
    toList = id

instance ToList (Join dbVendor) [Join dbVendor] where
    toList x = [x]

instance ToList [Join dbVendor] [Join dbVendor] where
    toList = id

instance ToList (Column colType dbVendor) [Column colType dbVendor] where
    toList x = [x]

instance ToList [Column colType dbVendor] [Column colType dbVendor] where
    toList = id

instance ToList (ColRef colType dbVendor) [ColRef colType dbVendor] where
    toList x = [x]

instance ToList [ColRef colType dbVendor] [ColRef colType dbVendor] where
    toList = id

instance ToList (ColWrap dbVendor) [ColWrap dbVendor] where
    toList x = [x]

instance ToList [ColWrap dbVendor] [ColWrap dbVendor] where
    toList = id

instance ToList (ColRefWrap dbVendor) [ColRefWrap dbVendor] where
    toList x = [x]

instance ToList [ColRefWrap dbVendor] [ColRefWrap dbVendor] where
    toList = id

instance ToList (SortRef dbVendor) [SortRef dbVendor] where
    toList x = [x]

instance ToList [SortRef dbVendor] [SortRef dbVendor] where
    toList = id
