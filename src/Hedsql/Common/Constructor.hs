-- file : Hedsql/Common/Constructor.hs
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    Constructors function.
    
    They provide an easier, more flexible and shorter way to create
    the various SQL data types.
    
    *Pre-requisites
    
    ** Deactivate the GHC OverloadedStrings language extension
    
    If you wish to provide parameters as simple strings,
    you should turn off the GHC OverloadedStrings language extension.
    
    > LANGUAGE NoOverloadedStrings
    
    This will allow to write queries like this:
    
    >     select "col1"
    > /++ from "table1"
    
    instead of:
    
    > select $ "col1"::String
    > from $ "table1"::String
    
    For more explanations related to the why and how, please refer to the
    following discussion:
    http://haskell.1045720.n5.nabble.com/Proposal-Improving-the-IsString-String
    -instance-td5734824.html
    
    ** Hiding prelude "and" function
    
    Since this constructor class also use a function named "and" as the Prelude
    package, it is recommended to hide the "and" function of the Prelude in the
    import.
    
    > import Prelude hiding (and)
    
    *Building a query
    
    The idea is to provide a limited set of functions with similar or close
    naming from SQL.
    Those functions have only a requested minimal set of arguments.
    The results of those functions can then be composed to form complete
    queries.
    For example, when using the 'select' function, you will not provide a FROM
    clause but only the arguments specific to the 'select' clause which are the
    columns.
    
    > select ["col1", "col2"]
    
    The additional FROM clause can be added using the '/++' function of the
    'Add' class.
    With our previous example, you could add a FROM part as so:
    > select ["col1", "col2"] /++ from "Table1"
    
    Thanks to type classes, those functions are polymorphic.
    It is therefore possible to pass different type of argument to the same
    functions. Let's take a look at the 'select' function:
    
    > select $ column "col1"
    > select "col1"
    > select [column "col1", column "col2"]
    > select ["col1", "col2"]
    
    All the above examples are valid.
    We can first see that it is possible to pass a single argument or a list to
    the 'select' function.
    It is also possible to pass arguments of the type 'Column' - returned by
    the 'column' function - or of type String.
    
    *Naming
    
    Most functions have the same name as their SQL functions counterpart.
    However, since some words are reserved in Haskell, the following functions
    have been renamed:
    - WHERE becomes 'w'
    - ALIAS becomes 'as' for table aliaising and 'out' for output aliaising.
    
    *Special cases
    
    **ORDER BY
    
    Limit and offset have to be added to the 'OrderBy' and are not part of a
    query on their own.
    This way, we ensure to have an ORDER BY clause defined when using OFFSET
    and LIMT, which is a good practice, because SQL does not guarantee any
    order of the result unless explicitely specified.
    This means that without an ORDER BY clause, using limit or offset would
    result in random results.
-}

module Hedsql.Common.Constructor where

import Hedsql.Common.DataStructure.Base
import Control.Lens
import Prelude hiding (and)

-- private functions

-- | Create a join on columns with a USING or ON clause.
columnJoin joinType table1 table2 clause =
    JoinColumn joinType (table table1) (table table2) (joinClause clause) Nothing
 
-- | Create a join on tables (CROSS or NATURAL join).
tableJoin joinType table1 table2 = JoinTable joinType (table table1) (table table2) Nothing

{-|
    Allow to easily add optional elements to data types using the '/++' infix function.
    
    For example, if you wish to add an ORDER BY clause to a SELECT query you can do it as follow:
    @
           selectQuery
     /++ orderByClause
    @
-}
class Add target elem where
    (/++) :: target -> elem -> target

-- | Add a LIMIT to an ORDER BY part.
instance Add OrderBy Limit where
    (/++) target elem = set partOrderByLimit (Just elem) target

-- | Add an OFFSET to an ORDER BY part.
instance Add OrderBy Offset where
    (/++) target elem = set partOrderByOffset (Just elem) target

-- | Add a table constraint to a CREATE TABLE statement.
instance Add CreateTable TableConstraint where
    (/++) target elem = target /++ [elem]

instance Add CreateTable [TableConstraint] where
    (/++) target elem = set createTableConstraints (Just elem) target

-- | Add a WHERE part to a DELETE query.
instance Add Delete Where where
    (/++) target elem = set deleteWherePart (Just elem) target

-- | Add a FROM part to a SELECT query.
instance Add SelectQuery From where
    (/++) target elem = set fromClause (Just elem) target

-- | Add a GROUP BY part to a SELECT query.
instance Add SelectQuery GroupBy where
    (/++) target elem = set groupByClause (Just elem) target

-- | Add an ORDER BY part to a SELECT query.
instance Add SelectQuery OrderBy where
    (/++) target elem = set orderByClause (Just elem) target

-- | Add a WHERE part to a SELECT query.
instance Add SelectQuery Where where
    (/++) target elem = set whereClause (Just elem) target

-- | Add a WHERE part to an UPDATE query.
instance Add Update Where where
    (/++) target elem = set updateWherePart (Just elem) target

-- | Add a Maybe value to a column.
instance Add Column a => Add Column (Maybe a) where
    (/++) target (Just elem) = target /++ elem
    (/++) target Nothing = target

-- | Add one constraint to a column.
instance Add Column ColConstraint where
    (/++) target elem = target /++ [elem]

-- | Add constraints to a column.
instance Add Column [ColConstraint] where
    (/++) target elem = set colConstraints (Just elem) target

-- | Add a column constraint type to a column.
instance Add Column ColConstraintType where
    (/++) target elem = target /++ colConstraint "" elem

-- | Add many column constraints types to a column.
instance Add Column [ColConstraintType] where
    (/++) target elems = target /++ map (colConstraint "") elems

-- | Specify the SQL data type of a column.
instance Add Column SqlDataType where
    (/++) target elem = set colDataType (Just elem) target

-- | Add a table to a column (to indicate that this column belongs to a specific table).
instance Add Column Table where
    (/++) target elem = set colTable (Just elem) target

class AssignedValueConstruct a where
    toAssignedVal :: a -> Expression
    toAssignedVals :: a -> [Expression]
    
instance ColRefConstruct a => AssignedValueConstruct a where
    toAssignedVal = toExpr
    toAssignedVals a = [toExpr a]

instance ColRefConstruct a => AssignedValueConstruct [a] where
    toAssignedVal = toAssignedVal.head
    toAssignedVals = map toAssignedVal

instance AssignedValueConstruct String where
    toAssignedVal a = ValueExpr (toValue a)
    toAssignedVals a = [toAssignedVal a]

instance AssignedValueConstruct [String] where
    toAssignedVal = toAssignedVal.head
    toAssignedVals = map toAssignedVal

-- | Create a column reference which can be used in SELECT clause.
class ColRefConstruct a where
    toColRef :: a -> ColRef
    toColRefs :: a -> [ColRef]

instance ColRefConstruct ColRef where
    toColRef a = a
    toColRefs a = [a]

instance ColRefConstruct [ColRef] where
    toColRef = head
    toColRefs a = a

instance ColRefConstruct Column where
    toColRef a = ColRef (ColExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [Column] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct CurrentDate where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct Function where
    toColRef a = ColRef (FuncExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct Int where
    toColRef a = ColRef (ValueExpr (toValue a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [Int] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct Operator where
    toColRef a = ColRef (OperatorExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [Operator] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct String where
    toColRef a = ColRef (ColExpr (column a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [String] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct Joker where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct SelectQuery where
    toColRef a = ColRef (SelectExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [SelectQuery] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct Random where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct SqlValue where
    toColRef a = ColRef (ValueExpr a) Nothing
    toColRefs a = map toColRef [a]

instance ColRefConstruct [SqlValue] where
    toColRef = toColRef.head
    toColRefs = map toColRef

instance ColRefConstruct Sum where
    toColRef a = ColRef (FuncExpr (Function a)) Nothing
    toColRefs a = map toColRef [a]

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

-- | Create table references.
class TableRefConstruct a where
    toTableRef :: a -> TableReference
    toTableRefs :: a -> [TableReference]

instance TableRefConstruct Join where
    toTableRef = TableJoinReference
    toTableRefs a = [toTableRef a]

instance TableRefConstruct Table where
    toTableRef a = TableTableReference a
    toTableRefs a = [toTableRef a]

instance TableRefConstruct [Table] where
    toTableRef = toTableRef.head
    toTableRefs = map toTableRef

instance TableRefConstruct TableReference where
    toTableRef a = a
    toTableRefs a = [a]

instance TableRefConstruct [TableReference] where
    toTableRef = head
    toTableRefs a = a

instance TableRefConstruct String where
    toTableRef = toTableRef.table
    toTableRefs = toTableRefs.table

instance TableRefConstruct [String] where
    toTableRef = toTableRef.table.head
    toTableRefs = toTableRefs.map table

-- | Create table aliases using AS.
class AliasConstruct a where
    alias :: a -> String -> a

instance AliasConstruct Table where
    alias table name = set tableAlias (Just (TableReferenceAlias name [])) table
    
instance AliasConstruct Join where
    alias join name =
            setAlias join name join
        where
            setAlias (JoinTable _ _ _ _) name = set joinTableAlias (Just name)
            setAlias (JoinColumn _ _ _ _ _) name = set joinColumnAlias (Just name)

class ConditionConstruct a where
    toCondition :: a -> Condition

instance ConditionConstruct Condition where
    toCondition a = a
    
instance ConditionConstruct FunctionBoolean where
    toCondition = FunctionCondition
   
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
    joinClause column = JoinClauseUsing [column]

-- | Create an USING join clause from a list of columns.
instance JoinClauseConstruct [Column] where
    joinClause columns = JoinClauseUsing columns

-- | Create an USING join clause from a string which is a column name.
instance JoinClauseConstruct String where
    joinClause colName = JoinClauseUsing [column colName]
    
instance JoinClauseConstruct [String] where
    joinClause colNames = JoinClauseUsing (columns colNames)

-- | Convert primitive values so they can be used in SQL queries as "raw" values.
class SqlValueConstructor a where
    toValue :: a -> SqlValue
    toValues :: a -> [SqlValue]
    
instance SqlValueConstructor Int where
    toValue = SqlValueInt
    toValues a = [toValue a]

instance SqlValueConstructor [Int] where
    toValue = toValue.head
    toValues = map toValue

instance SqlValueConstructor (Maybe a) where
    toValue Nothing = SqlValueNull
    toValues Nothing = [SqlValueNull]
    
instance SqlValueConstructor String where
    toValue = SqlValueString
    toValues a = [toValue a]

instance SqlValueConstructor [String] where
    toValue = toValue.head
    toValues = map toValue

instance SqlValueConstructor SqlValue where
    toValue a = a
    toValues a = [a]

instance SqlValueConstructor [SqlValue] where
    toValue = head
    toValues a = a

-- | Create a column which can then be used in a query.
class ColumnConstructor a where
    column :: a -> Column
    columns :: a -> [Column]

instance ColumnConstructor Char where
    column name = column [name]
    columns a = [column a]

instance ColumnConstructor Column where
    column a = a
    columns a = [a]
    
instance ColumnConstructor [Column] where
    column = head
    columns a = a
    
instance ColumnConstructor String where
    column name = Column name Nothing Nothing Nothing
    columns = map column
    
instance ColumnConstructor [String] where
    column = column.head
    columns = map column

-- | Create a table which can then be used in a query.
class TableConstructor a where
   table :: a -> Table

instance TableConstructor Table where
   table a = a
   
instance TableConstructor String where
   table name = Table name Nothing

-- | Create a column with a qualified name.
(/.) tName cName = set colTable (Just (table tName)) $ column cName

-- | "+" operator.
(/+) left right =  Add (toColRef left) (toColRef right)

-- | "-" operator.
(/-) left right =  Substract (toColRef left) (toColRef right)

-- | "*" operator.
(/*) left right =  Multiply (toColRef left) (toColRef right)

-- | Equality operator ("=" in SQL).
(/==) colRef1 colRef2 = Equal (toColRef colRef1) (toColRef colRef2)

-- | Unequality operator ("<>").
(/<>) colRef1 colRef2 = NotEqual (toColRef colRef1) (toColRef colRef2)

-- | Greater than operator (">").
(/>) colRef1 colRef2 = GreaterThan (toColRef colRef1) (toColRef colRef2)

-- | Greater than or equal to operator (">=").
(/>=) colRef1 colRef2 = GreaterThanOrEqualTo (toColRef colRef1) (toColRef colRef2)

-- | Smaller than operator ("<").
(/<) colRef1 colRef2 = SmallerThan (toColRef colRef1) (toColRef colRef2)

-- | Smaller than or equal to operator ("<=").
(/<=) colRef1 colRef2 = SmallerThanOrEqualTo (toColRef colRef1) (toColRef colRef2)

-- | Join two predicates with an AND.
and condition1 condition2 = And [toCondition condition1, toCondition condition2]

-- | Add an ascending sorting order (ASC) to a sort reference (which can be a column reference).
asc :: SortRefConstruct a => a -> SortRef
asc sortRef =  set sortRefOrder (Just Asc) (toSortRef sortRef)

-- | Add a descending sorting order (DESC) to a sort reference (which can be a column reference).
desc :: SortRefConstruct a => a -> SortRef
desc sortRef =  set sortRefOrder (Just Desc) (toSortRef sortRef)

-- | BETWEEN condition.
between expr lower higher =
    FunctionCondition (Between (toColRef expr) (toColRef lower) (toColRef higher))

-- | NOT BETWEEN condition.
notBetween expr lower higher =
    FunctionCondition (NotBetween (toColRef expr) (toColRef lower) (toColRef higher))

-- | Create a SQL Expression which can then be used in condition or column reference.
-- Use the ColRef class to avoid rewritting each instance.
toExpr a = toColRef a ^. colRefExpr
toExprs = map toExpr

-- | Create a CHECK constraint.
check :: ConditionConstruct a => a -> ColConstraintType
check condition = Check $ toCondition condition

-- | Create a constraint which shall then be applied on a column.
colConstraint :: String -> ColConstraintType -> ColConstraint
colConstraint name constraintType =
    ColConstraint (makeName name) constraintType
    where
        makeName "" = Nothing
        makeName name = Just name

-- | Create a COUNT function.
count expr = Count (toExpr expr)

-- | Create a CREATE TABLE statement.
createTable t cols = CreateTable False (table t) cols Nothing

-- | Create a CREATE TABLE IF NOT EXIST statement.
createTableIfNotExist t cols = CreateTable True (table t) cols Nothing

-- | Create a CREATE VIEW query.
createView ::
        [Char]            -- ^ Name of the view.
    -> SelectQuery -- ^ Select query from which the view is created.
    -> CreateView
createView name select = CreateView name select

-- | Create a CROSS JOIN.
crossJoin = tableJoin CrossJoin

-- | Create a DELETE FROM statement.
deleteFrom t = Delete (table t) Nothing

-- | Create a DROP TABLE statement.
dropTable :: String -- ^ Name of the table. 
                -> DropTable
dropTable tableName = DropTable False $ table tableName

dropTableIfExists tableName = DropTable True $ table tableName

-- | Create a DROP VIEW query.
dropView :: String -- ^ Name of the view.
                -> DropView
dropView = DropView

-- | Create an EXISTS function.
exists expr = FunctionCondition (Exists (toColRef expr))

-- | Create a FROM clause.
from a  = From (toTableRefs a)

-- | Create a FULL JOIN.
fullJoin = columnJoin FullJoin

-- | Create a random() function.
funcRandom = Random

-- | Create a GROUP BY clause.
groupBy cols = GroupBy (toColRefs cols) Nothing

-- | Add a HAVING clause to a GROUP BY clause.
having groupByClause clause = set groupByHaving (Just (toCondition clause)) groupByClause

-- | Create an IN operator.
i colRef1 colRef2 = In (toColRef colRef1) (toColRef colRef2)

-- | Create a NOT IN operator.
notIn colRef1 colRef2 = NotIn (toColRef colRef1) (toColRef colRef2)

-- | Create a INNER JOIN.
innerJoin = columnJoin InnerJoin

-- | Create an INSERT INTO statement.
insertInto t values = Insert (table t) Nothing (map toValues values)

insertIntoCols t cols values = Insert (table t) (Just (map column cols)) (map toValues values)

-- | Create a IS DISTINCT FROM operator.
isDistinctFrom colRef1 colRef2 = IsDistinctFrom (toColRef colRef1) (toColRef colRef2)

-- | Create a IS NOT DISTINCT FROM operator.
isNotDistinctFrom colRef1 colRef2 = IsNotDistinctFrom (toColRef colRef1) (toColRef colRef2)

-- | Create a IS FALSE function.
isFalse colRef = FunctionCondition (IsFalse (toColRef colRef))

-- | Create a IS NOT FALSE function.
isNotFalse colRef = FunctionCondition (IsNotFalse (toColRef colRef))

-- | Create a IS NOT NULL function.
isNotNull colRef = FunctionCondition (IsNotNull (toColRef colRef))

-- | Create a IS NOT TRUE function.
isNotTrue colRef = FunctionCondition (IsNotTrue (toColRef colRef))

-- | Create a IS NOT UNKNOWN function.
isNotUnknown colRef = FunctionCondition (IsNotUnknown (toColRef colRef))

-- | Create a IS NULL function.
isNull colRef = FunctionCondition (IsNull (toColRef colRef))

-- | Create a IS TRUE function.
isTrue colRef = FunctionCondition (IsTrue (toColRef colRef))

-- | Create a IS UNKNOWN function.
isUnknown colRef = FunctionCondition (IsUnknown (toColRef colRef))

-- | Create a subquery preceeded by LATERAL. 
lateral select alias = LateralTableReference select (TableReferenceAlias alias [])

-- | Create a LEFT JOIN.
leftJoin = columnJoin LeftJoin

-- | Create a LIMIT clause.
limit value = Limit value

-- | Create a MAX function.
max expr = Max (toExpr expr)

-- | Create a MIN function.
min expr = Min (toExpr expr)

-- | Create a NATURAL FULL JOIN.
naturalFullJoin = tableJoin NaturalFullJoin

-- | Create a NATURAL LEFT JOIN.
naturalLeftJoin = tableJoin NaturalLeftJoin

-- | Create a NATURAL INNER JOIN.
naturalInnerJoin = tableJoin NaturalInnerJoin

-- | Create a NATURAL RIGHT JOIN.
naturalRightJoin = tableJoin NaturalRightJoin

-- | Add a nulls first option (NULLS FIRST) to a sort reference (which can be a column reference).
nullsFirst :: SortRefConstruct a => a -> SortRef
nullsFirst sortRef =  set sortRefNulls (Just NullsFirst) (toSortRef sortRef)

-- | Add a nulls last option (NULLS LAST) to a sort reference (which can be a column reference).
nullsLast:: SortRefConstruct a => a -> SortRef
nullsLast sortRef =  set sortRefNulls (Just NullsLast) (toSortRef sortRef)

-- | Create an OFFSET clause.
offset value = Offset value

-- | Create an ORDER BY part of a query.
orderBy cols = OrderBy (toSortRefs cols) Nothing Nothing

-- | Create column reference label using AS.
out colRef name = set colRefLabel (Just name) colRef

-- | Create a RIGHT JOIN.
rightJoin = columnJoin RightJoin

-- | Create a SELECT query.
select a =
    SelectQuery (Select (toColRefs a) n) n n n n
    where
        n = Nothing

-- | Create a SELECT DISTINCT query.        
selectDistinct a =
    SelectQuery (Select (toColRefs a) (Just Distinct)) n n n n
    where
        n = Nothing

-- | Create a SELECT DISTINCT ON query.     
selectDistinctOn distinctExpr selectExpr =
    SelectQuery (Select (toColRefs selectExpr) distinctOn) n n n n
    where
        distinctOn = Just (DistinctOn (toExprs distinctExpr))
        n = Nothing

-- | Create a sub-query in a FROM clause.
subQuery select alias = SelectTableReference select (TableReferenceAlias alias [])

-- | Create an UPDATE statement.
update :: TableConstructor a => a -> [Assignment] -> Update
update t assignments = Update (table t) assignments Nothing

-- | Create an assignement to be used in an UPDATE statement.
assign :: (ColumnConstructor a, AssignedValueConstruct b) => a -> b -> Assignment
assign c value = Assignment (column c) (toAssignedVal value)

-- Column constraints

-- | Create a CHECK constraint to be used in a table constraint.
checkT condition = TableConstraintCheck $ toCondition condition

-- | Create a DEFAULT value constraint.
defaultValue expression = Default $ toExpr expression

-- | Create a FOREIGN KEY constraint.
foreignKey :: (TableConstructor a, ColumnConstructor b)
           => a -- ^ Table.
           -> b -- ^ Column.
           -> ColConstraintType
foreignKey t c = Reference (table t) (column c) Nothing

-- | Create a NOT NULL constraint.
notNull = NotNull

-- | Create a NULL constraint.
null = Null

-- | Create a NULL value.
nullVal = SqlValueNull

-- | Create a PRIMARY KEY constraint.
primary ::
        Bool -- ^ If True, the primary key will be an AUTOINCREMENT.
    -> ColConstraintType
primary isAutoIncrement = Primary isAutoIncrement

-- | Create a PRIMARY KEY constraint to be used in a table constraint.
primaryT cols = TableConstraintPrimaryKey (columns cols)

-- | Create an UNIQUE column constraint.
unique = Unique

-- | Create an UNIQUE table constraint
uniqueT cols = TableConstraintUnique (map column cols)

-- Sql data types constructors.

-- | Create a CHAR.
char length = SqlChar length

-- | Create a DATE.
date = Date

-- | Create a SMALLINT.
smallInt = SmallInt

-- | Create an INTEGER.
integer = Integer

-- | Create a BIGINT
bigInt = BigInt

-- | Create a VARCHAR.
varchar maxLength = Varchar maxLength

-- | Create a WHERE part.
w condition = Where (toCondition condition)

-- functions constructors.

-- | Create a joker - "*" - character.
(//*) = Joker

-- | Create a SUM function.
sum expr = Sum (toExpr expr)

class ConstructCombinedQuery a where
    toCombinedQuery :: a -> CombinedQuery
    
instance ConstructCombinedQuery SelectQuery where
    toCombinedQuery = CombinedQuerySingle
    
instance ConstructCombinedQuery CombinedQuery where
    toCombinedQuery a = a

except combinable1 combinable2 =
    CombinedQueryExcept [toCombinedQuery combinable1, toCombinedQuery combinable2]

exceptAll combinable1 combinable2 =
    CombinedQueryExceptAll [toCombinedQuery combinable1, toCombinedQuery combinable2]

intersect combinable1 combinable2 =
    CombinedQueryIntersect [toCombinedQuery combinable1, toCombinedQuery combinable2]

intersectAll combinable1 combinable2 =
    CombinedQueryIntersectAll [toCombinedQuery combinable1, toCombinedQuery combinable2]

-- | Create a table constraint.
tableConstraint name constraintType =
    TableConstraint (makeName name) constraintType Nothing
    where
        makeName "" = Nothing
        makeName name = Just name

-- | Create an UNION operation between two or more SELECT queries.
union combinable1 combinable2 =
    CombinedQueryUnion [toCombinedQuery combinable1, toCombinedQuery combinable2]

-- | Create an UNION ALL operation between two or more SELECT queries.
unionAll combinable1 combinable2 =
    CombinedQueryUnionAll [toCombinedQuery combinable1, toCombinedQuery combinable2]

{- |
    Create a function which will return the current date.
    Its implementation shall vary depending on the vendor.
-}
currentDate = CurrentDate