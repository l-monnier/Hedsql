-- file : Hedsql/Common/DefaultParser.hs

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  Default parser.

  Allow the creation of SQL queries strings from a SQL data structure.
  The generated strings will depends on the provided driver.
  Then, such a string can be used with a database interface such as HDBC.
  The big advantage is that the same SQL data structure can be used to generate
  strings dedicated to many different SQL backends such as PostgreSql or
  SqlLite. Therefore, you can write down your queries only once.
  
  Of course, some queries might be so specific to a backend that you would need
  a different one for that backend.
  In such a case you would need to create an additional class with your specific
  operations such as create, delete, etc. Then, you could implement a specific
  query for only this backend and keep the same for the others.
-}

module Hedsql.Common.DefaultParser where

import Control.Lens
import Data.Maybe
import Hedsql.Common.Driver
import Hedsql.Common.DataStructure.Base
import Hedsql.Common.Parser
import Hedsql.Common.Quoter
import Hedsql.Helpers.Patterns

{--------------------------------------------------------------------
    Private.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
    Public.
--------------------------------------------------------------------}
    
instance (DefaultParser Sql b) => Parser Sql b where
    toSqlString = toDefaultSqlString   
    
class AliasGetter a where
    getAliasClause :: b -> a -> String
    
instance AliasGetter Table where
    getAliasClause driver (Table _ (Just alias)) = getAliasClause driver alias
    getAliasClause driver (Table _ Nothing) = ""

instance AliasGetter TableReferenceAlias where
    getAliasClause driver (TableReferenceAlias name []) = " AS " ++ quoteSql driver name
    getAliasClause driver (TableReferenceAlias name cols) =
        "AS " ++ quoteSql driver name ++ getPatternFromList "(" ")" ", " (map (quoteSql driver) cols)

getColRefName driver expr = toSqlString driver $ expr^.colRefExpr

getColRefLabelName :: (Driver a, Parser a Expression) => a -> ColRef -> String
getColRefLabelName driver expr =
    stringMaybe l (quoteSql driver $ fromJust l) (getColRefName driver expr)
    where
        l = expr^.colRefLabel

-- Definition and instances of the query part.

class DefaultParser a b where
    toDefaultSqlString :: (Driver a) => a -> b -> String

instance (Parser a Column, Parser a Expression) => DefaultParser  a Assignment where
    toDefaultSqlString driver assignment =
           toSqlString driver (assignment^.assignmentCol)
        ++ " = "
        ++ toSqlString driver (assignment^.assignmentVal)

-- Build a timing constraint.
instance (
      Parser a ConstraintTimingCheck
    , Parser a ConstraintTimingType) => DefaultParser a ConstraintTiming where
    toDefaultSqlString driver timing =
             toSqlString driver (timing^.constraintTimingType)
        ++ toSqlString driver (timing^.constraintTimingCheck)

-- | Build a timing type constraint; "DEFERABLE" or "NOT DEFERABLE".
instance DefaultParser a ConstraintTimingType where
    toDefaultSqlString driver Deferable    = "DEFERABLE"
    toDefaultSqlString driver NotDeferable = "NOT DEFERABLE"

-- | Build a timing check constraint; "INITIALLY IMMEDIATE" or "INITIALLY DEFERRED"
instance DefaultParser a ConstraintTimingCheck where
    toDefaultSqlString driver InitiallyImmediate = "INITIALLY IMMEDIATE"
    toDefaultSqlString driver InitiallyDeferred  = "INITIALLY DEFERRED"

-- | Build a COUNT function.
instance Parser a Expression => DefaultParser  a Count where
    toDefaultSqlString driver (Count expr) = makeExpression driver "COUNT" expr

instance DefaultParser  a Joker where
    toDefaultSqlString _ Joker = "*"

instance Parser a Expression => DefaultParser  a Sum where
    toDefaultSqlString driver (Sum expr) = makeExpression driver "SUM" expr

instance Parser a Expression => DefaultParser a ColRef where
    toDefaultSqlString driver colRef =
           toSqlString driver (colRef^.colRefExpr)
        ++ stringMaybe name (" AS " ++ quoteSql driver (fromJust name)) ""
        where
            name = colRef^.colRefLabel

instance
    (
      Parser a CombinedQuery
    , Parser a SelectQuery
    )
    => DefaultParser a CombinedQuery 
    where
    toDefaultSqlString driver (CombinedQuerySingle select) =
        toSqlString driver select
    
    toDefaultSqlString driver (CombinedQueryExcept combinations) =
        makeCombinedQuery driver combinations "EXCEPT"
    
    toDefaultSqlString driver (CombinedQueryExceptAll combinations) =
        makeCombinedQuery driver combinations "EXCEPT ALL"
    
    toDefaultSqlString driver (CombinedQueryIntersect combinations) =
        makeCombinedQuery driver combinations "INTERSECT"
        
    toDefaultSqlString driver (CombinedQueryIntersectAll combinations) =
        makeCombinedQuery driver combinations "INTERSECT ALL"
    
    toDefaultSqlString driver (CombinedQueryUnion combinations) =
        makeCombinedQuery driver combinations "UNION"
        
    toDefaultSqlString driver (CombinedQueryUnionAll combinations) =
        makeCombinedQuery driver combinations "UNION ALL"

-- | Build a condition.
instance
    (
      Parser a Condition
    , Parser a FunctionBoolean
    )
    => DefaultParser a Condition
    where
    toDefaultSqlString driver (FunctionCondition condition) = toSqlString driver condition
    
    toDefaultSqlString driver (And conditions) =
        getPatternFromList "(" ")"  " AND " $ map (toSqlString driver) conditions
    
    toDefaultSqlString driver (Or conditions) =
        getPatternFromList "(" ")"  " OR " $ map (toSqlString driver) conditions

-- | Build a list of columns.
instance Parser a Column => DefaultParser a [Column] where
    toDefaultSqlString driver columns =
        getStringFromList (toSqlString driver) columns

{-|
    Build the name of a column.
    If the column has a table name defined,
    the qualified name ("tableName.columnName") will be returned.
    Else, the name of the column itself will be returned.
-}
instance Parser a Table => DefaultParser a Column where    
    toDefaultSqlString driver (Column name _ _ (Just table)) =
        toSqlString driver table ++ "." ++ quoteSql driver name
    
    toDefaultSqlString driver (Column name _ _ Nothing) = quoteSql driver name

-- | Build a SQL expression.
instance (
      Parser a Column
    , Parser a Function
    , Parser a Operator
    , Parser a SelectQuery
    , Parser a SqlValue
    ) => DefaultParser a Expression where
   
   toDefaultSqlString driver (ColExpr col) = toSqlString driver col
   toDefaultSqlString driver (SelectExpr select) = "(" ++ toSqlString driver select ++ ")"   
   toDefaultSqlString driver (FuncExpr func) = toSqlString driver func
   toDefaultSqlString driver (OperatorExpr operator) = toSqlString driver operator
   toDefaultSqlString driver (ValueExpr value) = toSqlString driver value
   toDefaultSqlString driver (ValueExprs values) = getPatternFromList "(" ")" ", " $ map (toSqlString driver) values

instance Parser a ColConstraintType => DefaultParser a ColConstraint where
    toDefaultSqlString driver colConstraint = 
           makeName (colConstraint^.colConstraintName)
        ++ toSqlString driver (colConstraint^.colConstraintType)
        where
            makeName (Just name) = "CONSTRAINT " ++ quoteSql driver name ++ " "
            makeName Nothing = ""

-- | Build a column constraint.
instance (
      Parser a Condition
    , Parser a Expression
    , Parser a FunctionBoolean
    , Parser a OnAction
    , Parser a Table) => DefaultParser a ColConstraintType where
    
    toDefaultSqlString driver (Check condition) =
        "CHECK (" ++ toSqlString driver condition ++ ")"
    toDefaultSqlString driver (Default val) =
        "DEFAULT(" ++ toSqlString driver val ++ ")"
    toDefaultSqlString _ NotNull = "NOT NULL"
    toDefaultSqlString _ Null = "NULL"
    
    -- | Build a PRIMARY KEY constraint.
    toDefaultSqlString driver (Primary isAutoIncrement) =
        "PRIMARY KEY" ++ makeAutoIncrement isAutoIncrement
        where
            makeAutoIncrement True = " AUTOINCREMENT"
            makeAutoIncrement False = ""
    
    
    toDefaultSqlString driver (Reference table column action) =
           "REFERENCES "
        ++ toSqlString driver table
        ++ "("
        ++ quoteSql driver (column^.colName)
        ++ ")"
        ++ makeAction action
        where
            makeAction (Just action) = " " ++ toSqlString driver action
            makeAction Nothing = ""
    
    toDefaultSqlString _ Unique = "UNIQUE"

-- | Build a FOREIGN KEY clause.
instance (
      Parser a Column
    , Parser a OnAction
    , Parser a Match
    , Parser a Table) => DefaultParser a ForeignKeyClause where
    toDefaultSqlString driver fk =
             toSqlString driver (fk^.foreignKeyClauseTable)
        ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) (fk^.foreignKeyClauseCols))
        ++ makeMatch driver (fk^.foreignKeyMatch)
        ++ makeAction driver (fk^.foreignKeyClauseAction)
        where
            makeMatch driver (Just match) = " " ++ toSqlString driver match
            makeMatch driver Nothing = ""
            makeAction driver (Just action) = " " ++ toSqlString driver action
            makeAction driver Nothing = ""

-- | Build a function returning a boolean value.
instance Parser a ColRef => DefaultParser a FunctionBoolean where
    
    toDefaultSqlString driver (Between expr lower higher) =
             "("
        ++ toSqlString driver expr
        ++ " BETWEEN "
        ++ toSqlString driver lower
        ++ " AND "
        ++ toSqlString driver higher
        ++ ")"
    
    toDefaultSqlString driver (Exists expr) = "(EXISTS " ++ toSqlString driver expr ++ ")"

    -- | Build a IS FALSE function.
    toDefaultSqlString driver (IsFalse expr) = toSqlString driver expr ++ " IS FALSE"

    -- | Build a IS NOT FALSE function.
    toDefaultSqlString driver (IsNotFalse expr) = toSqlString driver expr ++ " IS NOT FALSE"

    -- | Build a IS NOT NULL function.
    toDefaultSqlString driver (IsNotNull expr) = toSqlString driver expr ++ " IS NOT NULL"

    -- | Build a IS NOT TRUE function.
    toDefaultSqlString driver (IsNotTrue expr) = toSqlString driver expr ++ " IS NOT TRUE"

    -- | Build a IS NOT UNKNOWN function.
    toDefaultSqlString driver (IsNotUnknown expr) = toSqlString driver expr ++ " IS NOT UNKNOWN"

    -- | Build a IS NULL function.
    toDefaultSqlString driver (IsNull expr) = toSqlString driver expr ++ " IS NULL"

    -- | Build a IS TRUE function.
    toDefaultSqlString driver (IsTrue expr) = toSqlString driver expr ++ " IS TRUE"

    -- | Build a IS UNKNOWN function.
    toDefaultSqlString driver (IsUnknown expr) = toSqlString driver expr ++ " IS UNKNOWN"

    toDefaultSqlString driver (NotBetween expr lower higher) =
             "("
        ++ toSqlString driver expr
        ++ " NOT BETWEEN "
        ++ toSqlString driver lower
        ++ " AND "
        ++ toSqlString driver higher
        ++ ")"
        
    toDefaultSqlString driver (Equal ref1 ref2) = makeInfixFunction driver "=" ref1 ref2
    
    toDefaultSqlString driver (GreaterThan ref1 ref2) = makeInfixFunction driver ">" ref1 ref2
    
    toDefaultSqlString driver (GreaterThanOrEqualTo ref1 ref2) =
        makeInfixFunction driver ">=" ref1 ref2
    
    toDefaultSqlString driver (In ref1 ref2) = makeInfixFunction driver "IN" ref1 ref2
    
    toDefaultSqlString driver (IsDistinctFrom ref1 ref2) =
        makeInfixFunction driver "IS DISTINCT FROM" ref1 ref2
    
    toDefaultSqlString driver (IsNotDistinctFrom ref1 ref2) =
        makeInfixFunction driver "IS NOT DISTINCT FROM" ref1 ref2
    
    toDefaultSqlString driver (Like ref1 ref2) = makeInfixFunction driver "LIKE" ref1 ref2
    
    toDefaultSqlString driver (NotEqual ref1 ref2) = makeInfixFunction driver "<>" ref1 ref2
    
    toDefaultSqlString driver (NotIn ref1 ref2) = makeInfixFunction driver "NOT IN" ref1 ref2
    
    toDefaultSqlString driver (SmallerThan ref1 ref2) = makeInfixFunction driver "<" ref1 ref2
    
    toDefaultSqlString driver (SmallerThanOrEqualTo ref1 ref2) =
        makeInfixFunction driver "<=" ref1 ref2

-- | Build the ON or USING clause of a JOIN.
instance (Parser a Column, Parser a Condition) => DefaultParser a JoinClause where
    toDefaultSqlString driver (JoinClauseOn predicate) = " ON " ++ toSqlString driver predicate
    toDefaultSqlString driver (JoinClauseUsing columns) =
        getPatternFromList " USING(" ")" ", " $ map (toSqlString driver) columns

-- | Build a LIMIT clause.
instance DefaultParser a Limit where
    toDefaultSqlString driver limit = "LIMIT " ++ (show $ limit^.limitValue)

-- | Build a MAX function.
instance Parser a Expression => DefaultParser  a Max where
    toDefaultSqlString driver (Max expr) = makeExpression driver "Max" expr

-- | Build a MIN function.
instance Parser a Expression => DefaultParser  a Min where
    toDefaultSqlString driver (Min expr) = makeExpression driver "Min" expr

-- | Build a OFFSET clause.
instance DefaultParser a Offset where
    toDefaultSqlString driver offset = "OFFSET " ++ (show $ offset^.offsetValue)

-- | Build a MATCH clause.
instance DefaultParser a Match where
    toDefaultSqlString driver Full =  "FULL"
    toDefaultSqlString driver Partial =  "PARTIAL"
    toDefaultSqlString driver Simple = "SIMPLE"

-- | Build actions expressions such as ON DELETE or ON UPDATE clauses.
instance Parser a SqlAction => DefaultParser a OnAction where
    toDefaultSqlString driver (OnDelete action) = "ON DELETE " ++ toSqlString driver action
    toDefaultSqlString driver (OnUpdate action) = "ON UPDATE " ++ toSqlString driver action

-- | Build operators such as "+" or "*".
instance Parser a ColRef => DefaultParser a Operator where
    toDefaultSqlString driver (Add left right) = "(" ++ toSqlString driver left ++ " + " ++ toSqlString driver right ++ ")"
    toDefaultSqlString driver (Multiply left right) = "(" ++ toSqlString driver left ++ " * " ++ toSqlString driver right ++ ")"
    toDefaultSqlString driver (Substract left right) = "(" ++ toSqlString driver left ++ " - " ++ toSqlString driver right ++ ")"

-- | Build a FROM part.
instance Parser a [TableReference] => DefaultParser a From where
   toDefaultSqlString driver (From tableReferences) =
       "FROM " ++ getReferences driver tableReferences
       where
           getReferences driver [TableTableReference table] = quoteSql driver $ table^.tableName
           getReferences driver tableRef = toSqlString driver tableRef

instance (Parser a Condition, Parser a Expression) => DefaultParser a GroupBy where
  toDefaultSqlString driver (GroupBy colRefs condition) =
             getPatternFromList "GROUP BY " "" ", " (map (getColRefName driver) colRefs)
       ++  stringMaybe condition (" HAVING " ++ toSqlString driver (fromJust condition)) ""

-- | Build an ORDER BY part.
instance (
      Parser a Expression
    , Parser a Limit
    , Parser a Offset
    ) => DefaultParser a OrderBy
    where
   toDefaultSqlString driver orderByClause =
             getPatternFromList "ORDER BY " "" ", " (map (toDefaultSqlString driver) sortRefs)
        ++ (makeLimit $ orderByClause^.partOrderByLimit)
        ++ (makeOffset $ orderByClause^.partOrderByOffset)
        where
            sortRefs = orderByClause^.partOrderByColumns
            makeLimit Nothing = ""
            makeLimit (Just limit) = " " ++ toSqlString driver limit
            makeOffset Nothing = ""
            makeOffset (Just offset) = " " ++ toSqlString driver offset

instance (Parser a Expression) => DefaultParser a SortRef where
    toDefaultSqlString driver sortRef =
             getColRefLabelName driver colRef
        ++ (makeOrder $ sortRef^.sortRefOrder)
        ++ (makeNull $ sortRef^.sortRefNulls)
        where
            colRef = sortRef^.sortRefCol
            makeOrder Nothing= ""
            makeOrder (Just order) = " " ++ toDefaultSqlString driver order
            makeNull Nothing = ""
            makeNull (Just null) = " " ++ toDefaultSqlString driver null

instance DefaultParser a SortOrder where
    toDefaultSqlString driver Asc = "ASC"
    toDefaultSqlString driver Desc = "DESC"

instance DefaultParser a SortNulls where
    toDefaultSqlString driver NullsFirst = "NULLS FIRST"
    toDefaultSqlString driver NullsLast = "NULLS LAST"

-- | Build a SELECT part.
instance (Parser a ColRef, Parser a Expression) => DefaultParser a Select where
  toDefaultSqlString driver (Select expressions (Just Distinct)) =
       getPatternFromList "SELECT DISTINCT " "" ", " $ map (toSqlString driver) expressions
   
  toDefaultSqlString driver (Select expressions (Just (DistinctOn exprs))) =
          getPatternFromList "SELECT DISTINCT ON (" ")" ", " (map (toSqlString driver) exprs)
       ++ getPatternFromList " " "" ", " (map (toSqlString driver) expressions)
   
  toDefaultSqlString driver (Select expressions _) =
       getPatternFromList "SELECT " "" ", " $ map (toSqlString driver) expressions
       
-- | Build a WHERE part.
instance Parser a Condition => DefaultParser a Where where
   toDefaultSqlString driver (Where condition) = "WHERE " ++ toSqlString driver condition
    
-- | Build a CREATE statement.   
instance (
      Parser a ColConstraint
    , Parser a Column
    , Parser a SqlDataType
    , Parser a TableConstraint
    , Parser a Table) => DefaultParser a CreateTable where
    
    toDefaultSqlString driver stmt = makeCreateStatement driver stmt makeColumn

-- | Create a CREATE VIEW statement.
instance Parser a SelectQuery => DefaultParser a CreateView where
   toDefaultSqlString driver statement =
             "CREATE VIEW "
        ++ quoteSql driver (statement^.viewName)
        ++ " AS "
        ++ toSqlString driver (statement^.viewSelect)

-- | Build a DELETE query.
instance (Parser a Table, Parser a Where) => DefaultParser a Delete where
   toDefaultSqlString driver query =
             "DELETE "
        ++ toSqlString driver (query^.deleteTable)
        ++ (makeWhere driver $ query^.deleteWherePart)

-- | Creates a DROP TABLE statement.
instance Parser a Table => DefaultParser a DropTable where
   toDefaultSqlString driver statement =
            "DROP TABLE "
       ++ makeIfExists (statement^.dropTableIfExistsParam)
       ++ toSqlString driver (statement^.dropTableTable)
       where
           makeIfExists True = "IF EXISTS "
           makeIfExists False = ""

-- | Build a DROP VIEW query.
instance DefaultParser a DropView where
   toDefaultSqlString driver query = "DROP VIEW " ++ (quoteSql driver $ query^.dropViewName)

-- | Create an INSERT query.
instance (Parser a Column, Parser a SqlValue) => DefaultParser a Insert where
   toDefaultSqlString driver query =
             "INSERT INTO " ++ query^.insertTable.tableName
        ++ makeCols (query^.insertColumns)
        ++ " VALUES " ++  params
        where
            makeCols (Just columns) = getPatternFromList " (" ")" ", " $ map (toSqlString driver) columns
            makeCols Nothing = ""
            params =
                getPatternFromList "" "" ", " $ map (\xs -> getPatternFromList "(" ")" ", " $ map (toSqlString driver) $ xs) (query^.insertValues) 

-- | Build a SELECT query.
instance (
      Parser a GroupBy
    , Parser a From
    , Parser a OrderBy
    , Parser a Select
    , Parser a Where
    ) => DefaultParser a SelectQuery
    where
   toDefaultSqlString driver query =
             toSqlString driver (query^.selectClause)
        ++ makeMaybe driver (query^.fromClause)
        ++ (makeWhere driver $ _whereClause query)
        ++ makeMaybe driver (query^.groupByClause)
        ++ makeMaybe driver (query^.orderByClause)
        where
            makeMaybe driver (Just a) = " " ++ toSqlString driver a
            makeMaybe driver Nothing = ""

-- | Build an UPDATE query.
instance (
      Parser a Assignment
    , Parser a Where
    , Parser a SqlValue
    ) => DefaultParser a Update where
    toDefaultSqlString driver query =
              "UPDATE " ++ (query^.updateTable.tableName)
         ++ " SET" ++ assignments
         ++ (makeWhere driver $ query^.updateWherePart)
         where
             assignments = getPatternFromList " " "" ", " $ map (toSqlString driver) $ query^.updateAssignments

-- | Build a SQL action such as CASCADE and RESTRICT.
instance DefaultParser a SqlAction where
   toDefaultSqlString driver Cascade = "CASCADE"
   toDefaultSqlString driver Restrict = "RESTRICT"

-- | Build the SQL data types.
instance DefaultParser a SqlDataType where
    toDefaultSqlString _ Date = "date"
    toDefaultSqlString _ (SqlChar lenght) = "char(" ++ show lenght ++ ")"
    toDefaultSqlString _ SmallInt = "smallint"
    toDefaultSqlString _ Integer = "integer" --"integer(" ++ show size ++ ")"
    toDefaultSqlString _ BigInt = "bigint"
    toDefaultSqlString _ (Varchar max) = "varchar(" ++ show max ++ ")"
    
-- | Build and quote input values.
instance DefaultParser a SqlValue where
    toDefaultSqlString _ (SqlValueInt int) = show int
    
    -- TODO: implements quoting.
    toDefaultSqlString _ (SqlValueString string) = "'" ++ string ++ "'"
    
    toDefaultSqlString _ SqlValueDefault = "DEFAULT"
    
    toDefaultSqlString _ SqlValueNull = "NULL"

-- | Build a table.
instance DefaultParser a Table where
    toDefaultSqlString driver table =
        quoteSql driver $ getTableName table
        where
            getTableName (Table _ (Just alias)) = alias^.tableReferenceAliasName
            getTableName (Table name Nothing) = name

-- | Build a table constraint.
instance (
      Parser a ConstraintTiming
    , Parser a TableConstraintType) => DefaultParser a TableConstraint where
    toDefaultSqlString driver table =
             makeName (table^.tableConstraintName)
        ++ toSqlString driver (table^.tableConstraintConstraint)
        ++ makeTiming driver (table^.tableConstraintTiming)
        where
            -- TODO: check if constraints name can be quoted.
            makeName (Just name) = "CONSTRAINT " ++ quoteSql driver name ++ " "
            makeName Nothing = ""
            makeTiming driver (Just timing) = " " ++ toSqlString driver timing
            makeTiming driver Nothing = ""

-- Build a table constraint type.
instance (
      Parser a Column
    , Parser a Condition
    , Parser a ForeignKeyClause) => DefaultParser a TableConstraintType where
    toDefaultSqlString driver (TableConstraintCheck condition) =
             "Check" ++ toSqlString driver condition
    
    toDefaultSqlString driver (ForeignKey cols clause) =
             "FOREIGN KEY"
        ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) cols)
        ++ toSqlString driver clause
    
    toDefaultSqlString driver (TableConstraintPrimaryKey cols) =
             "PRIMARY KEY" ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) cols)
        
    toDefaultSqlString driver (TableConstraintUnique cols) =
             "UNIQUE" ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) cols)
    

-- | Build joins.
instance (
      Parser a JoinTypeColumn
    , Parser a JoinClause
    , Parser a JoinTypeTable
    ) => DefaultParser a Join
    where

    toDefaultSqlString driver join = 
        getJoin driver join
        where
            getJoin driver (JoinColumn joinType table1 table2 clause alias) =
                     stringIf (isJust alias) "("
                ++ commonPart driver joinType table1 table2
                ++ toSqlString driver clause
                ++ getAlias driver alias
            getJoin driver (JoinTable joinType table1 table2 alias) =
                     stringIf (isJust alias) "("
                ++ commonPart driver joinType table1 table2
                ++ getAlias driver alias
                
            -- Common part between column and table joins.
            commonPart driver joinType table1 table2 =
                     quoteSql driver (table1^.tableName) ++ (getAliasClause driver table1)
                ++ " " ++ toSqlString driver joinType ++ " "
                ++ quoteSql driver (table2^.tableName) ++ (getAliasClause driver table2)
            
            -- Generate the parenthesis closure and the alias clause for the join.
            getAlias driver alias = stringIf (isJust alias) (") AS " ++ quoteSql driver (fromJust alias))
            

instance DefaultParser a JoinTypeColumn where
    toDefaultSqlString driver FullJoin = "FULL JOIN"
    toDefaultSqlString driver LeftJoin = "LEFT JOIN"
    toDefaultSqlString driver InnerJoin = "INNER JOIN"
    toDefaultSqlString driver RightJoin = "RIGHT JOIN"

instance DefaultParser a JoinTypeTable where
    toDefaultSqlString driver CrossJoin = "CROSS JOIN"
    toDefaultSqlString driver NaturalFullJoin = "NATURAL FULL JOIN"
    toDefaultSqlString driver NaturalLeftJoin = "NATURAL LEFT JOIN"
    toDefaultSqlString driver NaturalInnerJoin = "NATURAL INNER JOIN"
    toDefaultSqlString driver NaturalRightJoin = "NATURAL RIGHT JOIN"

-- | Build a table reference for the use in a FROM clause.
instance (
      Parser a Join
    , Parser a SelectQuery
    , Parser a Table
    ) => DefaultParser a TableReference
    where
    toDefaultSqlString driver (TableJoinReference reference) = toSqlString driver reference
    toDefaultSqlString driver (TableTableReference table) = toSqlString driver table
    toDefaultSqlString driver (SelectTableReference select alias) =
        "(" ++ toSqlString driver select ++ ")" ++ getAliasClause driver alias
    toDefaultSqlString driver (LateralTableReference select alias) =
        "LATERAL (" ++ toSqlString driver select ++ ")" ++ getAliasClause driver alias

-- | Build a list of table references for the use in a FROM clause.
instance (Parser a TableReference) => DefaultParser a [TableReference] where
    toDefaultSqlString driver references =
        getStringFromList (toSqlString driver) references

{-| Apply a string function to a list of elements and return each of them as
    strings separated by commas.
-}
getStringFromList::
       (a -> String)
    -> [a]
    -> String
getStringFromList function list =
    getPatternFromList "" "" ", " $ map function list

-- | Build a column which can be used for a CREATE statement.
makeColumn
    :: (Driver a, Parser a SqlDataType, Parser a ColConstraint) =>
       a -> Column -> [Char]
makeColumn driver column =
       quoteSql driver (column^.colName)
    ++ makeDataType driver (column^.colDataType)
    ++ makeColumnConstraints driver (column^.colConstraints)

-- | Build column constraints.
makeColumnConstraints :: (Driver a, Parser a b) => a -> Maybe [b] -> [Char]
makeColumnConstraints driver (Just constraints) =
            getPatternFromList " " "" " " $ map (toSqlString driver) constraints
makeColumnConstraints driver Nothing = ""   

-- | Build a combined query.
makeCombinedQuery driver combinations t =
    getPatternFromList "(" ")" (" " ++ t ++ " ") $ map (toSqlString driver) combinations

-- | Build a create statement.
makeCreateStatement driver stmt makeColumnsFunc =
   "CREATE TABLE "
        ++ table
        ++ " ("
        ++ columns
        ++ constraints
        ++ ")"
        where
            columns =
                getStringFromList (makeColumnsFunc driver) (stmt^.createTableCols)
            constraints = makeConstraints driver (stmt^.createTableConstraints)
            table = toSqlString driver (stmt^.createTableTable)
            makeConstraints driver (Just constraints) =
               ", " ++ getStringFromList (toSqlString driver) constraints
            makeConstraints driver Nothing = ""

-- | Build a data type.
makeDataType :: (Driver a, Parser a b) => a -> Maybe b -> [Char]
makeDataType driver (Just dataType) = " " ++ toSqlString driver dataType
makeDataType driver Nothing = ""

-- | Build an expression.
makeExpression driver string expression = string ++ "(" ++ toSqlString driver expression ++ ")"

-- | Build a where part from a Maybe WherePart.
makeWhere driver (Just whereClause) = " " ++ toSqlString driver whereClause
makeWhere driver Nothing = ""

-- | Build an infix function.
makeInfixFunction driver name ref1 ref2 =
         toSqlString driver ref1
    ++ " " ++ name ++ " "
    ++ toSqlString driver ref2

-- | Build a function.
instance (
      Parser a Count
    , Parser a CurrentDate
    , Parser a Joker
    , Parser a Max
    , Parser a Min
    , Parser a Random
    , Parser a Sum
    ) => DefaultParser a Function where
    toDefaultSqlString a (Function b) = toSqlFunctionString a b

instance DefaultParser a CurrentDate where
    toDefaultSqlString _  _ = "CURRENT_DATE"
    
instance DefaultParser a Random where
    toDefaultSqlString _  _ = "RANDOM"