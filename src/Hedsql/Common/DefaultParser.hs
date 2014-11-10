{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Hedsql/Common/DefaultParser.hs
Description : Default SQL parser.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Allow the creation of SQL queries strings from a SQL data structure.
The generated strings will depends on the constructor used to build the
structure. Such a string can be used with a database interface such as HDBC.

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

import Hedsql.Common.Driver
import Hedsql.Common.DataStructure.Base
import Hedsql.Common.Parser
import Hedsql.Common.Quoter
import Hedsql.Helpers.Patterns

import Control.Lens
import Data.Maybe

-- Types.

data P a = P
    {
      _funcParser      :: FuncParser a
    , _quoter          :: Q a
    }

-- Make the lenses.
makeLenses ''P
makeLenses ''FuncParser
makeLenses ''Q

-- Create the default parser and quoter.

defaultParser :: P a
defaultParser = P
    parseColFunc
    parseColRefFunc
    parseColRefDefFunc
    parseConditionFunc
    parseFuncFunc
    parseFuncBoolFunc
    parseDeleteFunc
    parseDropTableFunc
    parseDropViewFunc
    parseExprFunc
    parseFromFunc
    parseGroupByFunc
    parseJoinFunc
    parseJoinClauseFunc
    parseJoinTColFunc
    parseJoinTTableFunc
    parseOrderByFunc
    parseOperatorFunc
    parseSortNullFunc
    parseSelectFunc
    parseWhereFunc
    parseSortRefFunc
    parseTableNameFunc
    parseTableRefFunc
    parseTableRefAsFunc
    parseValueFunc
    defaultFuncParser
    defaultQuoter

defaultFuncParser :: FuncParser a
defaultFuncParser = FuncParser
    parseCountFunc
    parseCurrentDateFunc
    parseMaxFunc
    parseMinFunc
    parseJokerFunc
    parseRandomFunc
    parseSumFunc 

-- Private.

{-|
Helper function which provides the parser directly to the lens function.

To make it clearer, if you have a parser "p" and this parser has the parse
function lense "parseThing", instead of writing:
> (p^.parseThing) parser thingToParse

You can now write:
> (p^%parseThing) thingToParse
-}
(^%) :: a -> Getting (a -> b) a (a -> b) -> b
(^%) parser b = parser^.b $ parser

-- | Parse an order (ASC or DESC).
parseOrder :: Maybe SortOrder -> String
parseOrder Nothing     = ""
parseOrder (Just Asc)  = "ASC"
parseOrder (Just Desc) = "DESC"

-- Public.

{-|
Instances of this class are the elements which can be parsed.
Such elements are specific to a given driver type (which is a phantom type).
For example: (DropTable PostgreSQL).

Therefore, the instances of this class are located in the specific driver
implementation.
-}
class Parseable a where
    parse :: a -> String


-- Old code.

instance (DefaultParser Sql b) => Parser Sql b where
    toSqlString = toDefaultSqlString   

-- Definition and instances of the query part.

class DefaultParser a b where
    toDefaultSqlString :: (Driver a) => a -> b -> String

-- instance (Parser a Column, Parser a Expression) => DefaultParser  a Assignment where
--    toDefaultSqlString driver assignment =
--           toSqlString driver (assignment^.assignmentCol)
--        ++ " = "
--        ++ toSqlString driver (assignment^.assignmentVal)

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

--instance
--    (
--      Parser a CombinedQuery
--    , Parser a Select
--    )
--    => DefaultParser a CombinedQuery 
--    where
--    toDefaultSqlString driver (CombinedQuerySingle select) =
--        toSqlString driver select
--    
--    toDefaultSqlString driver (CombinedQueryExcept combinations) =
--        makeCombinedQuery driver combinations "EXCEPT"
--    
--    toDefaultSqlString driver (CombinedQueryExceptAll combinations) =
--        makeCombinedQuery driver combinations "EXCEPT ALL"
--    
--    toDefaultSqlString driver (CombinedQueryIntersect combinations) =
--        makeCombinedQuery driver combinations "INTERSECT"
--        
--    toDefaultSqlString driver (CombinedQueryIntersectAll combinations) =
--        makeCombinedQuery driver combinations "INTERSECT ALL"
--    
--    toDefaultSqlString driver (CombinedQueryUnion combinations) =
--        makeCombinedQuery driver combinations "UNION"
--        
--    toDefaultSqlString driver (CombinedQueryUnionAll combinations) =
--        makeCombinedQuery driver combinations "UNION ALL"


--instance Parser a ColConstraintType => DefaultParser a ColConstraint where
--    toDefaultSqlString driver colConstraint = 
--           makeName (colConstraint^.colConstraintName)
--        ++ toSqlString driver (colConstraint^.colConstraintType)
--        where
--            makeName (Just name) = "CONSTRAINT " ++ quoteSql driver name ++ " "
--            makeName Nothing = ""

-- | Build a column constraint.
--instance (
--      Parser a Condition
--    , Parser a Expression
--    , Parser a FuncBool
--    , Parser a OnAction
--    , Parser a Table) => DefaultParser a ColConstraintType where
--    
--    toDefaultSqlString driver (Check condition) =
--        "CHECK (" ++ toSqlString driver condition ++ ")"
--    toDefaultSqlString driver (Default val) =
--        "DEFAULT(" ++ toSqlString driver val ++ ")"
--    toDefaultSqlString _ NotNull = "NOT NULL"
--    toDefaultSqlString _ Null = "NULL"
--    
--    -- | Build a PRIMARY KEY constraint.
--    toDefaultSqlString driver (Primary isAutoIncrement) =
--        "PRIMARY KEY" ++ makeAutoIncrement isAutoIncrement
--        where
--            makeAutoIncrement True = " AUTOINCREMENT"
--            makeAutoIncrement False = ""
--    
--    
--    toDefaultSqlString driver (Reference table column action) =
--           "REFERENCES "
--        ++ toSqlString driver table
--        ++ "("
--        ++ quoteSql driver (column^.colName)
--        ++ ")"
--        ++ makeAction action
--        where
--            makeAction (Just action) = " " ++ toSqlString driver action
--            makeAction Nothing = ""
--    
--    toDefaultSqlString _ Unique = "UNIQUE"

-- | Build a FOREIGN KEY clause.
--instance (
--      Parser a Column
--    , Parser a OnAction
--    , Parser a Match
--    , Parser a Table) => DefaultParser a ForeignKeyClause where
--    toDefaultSqlString driver fk =
--             toSqlString driver (fk^.foreignKeyClauseTable)
--        ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) (fk^.foreignKeyClauseCols))
--        ++ makeMatch driver (fk^.foreignKeyMatch)
--        ++ makeAction driver (fk^.foreignKeyClauseAction)
--        where
--            makeMatch driver (Just match) = " " ++ toSqlString driver match
--            makeMatch driver Nothing = ""
--            makeAction driver (Just action) = " " ++ toSqlString driver action
--            makeAction driver Nothing = ""

-- | Build a MATCH clause.
instance DefaultParser a Match where
    toDefaultSqlString driver Full =  "FULL"
    toDefaultSqlString driver Partial =  "PARTIAL"
    toDefaultSqlString driver Simple = "SIMPLE"

-- | Build actions expressions such as ON DELETE or ON UPDATE clauses.
instance Parser a SqlAction => DefaultParser a OnAction where
    toDefaultSqlString driver (OnDelete action) = "ON DELETE " ++ toSqlString driver action
    toDefaultSqlString driver (OnUpdate action) = "ON UPDATE " ++ toSqlString driver action
    
-- | Build a CREATE statement.   
--instance (
--      Parser a ColConstraint
--    , Parser a Column
--    , Parser a SqlDataType
--    , Parser a TableConstraint
--    , Parser a Table) => DefaultParser a CreateTable where
--    
--    toDefaultSqlString driver stmt = makeCreateStatement driver stmt makeColumn

-- | Create a CREATE VIEW statement.
--instance Parser a Select => DefaultParser a CreateView where
--   toDefaultSqlString driver statement =
--             "CREATE VIEW "
--        ++ quoteSql driver (statement^.viewName)
--        ++ " AS "
--        ++ toSqlString driver (statement^.viewSelect)

-- | Create an INSERT query.
--instance (Parser a Column, Parser a SqlValue) => DefaultParser a Insert where
--   toDefaultSqlString driver query =
--             "INSERT INTO " ++ query^.insertTable.tableName
--        ++ makeCols (query^.insertColumns)
--        ++ " VALUES " ++  params
--        where
--            makeCols (Just columns) = getPatternFromList " (" ")" ", " $ map (toSqlString driver) columns
--            makeCols Nothing = ""
--            params =
--                getPatternFromList "" "" ", " $ map (\xs -> getPatternFromList "(" ")" ", " $ map (toSqlString driver) $ xs) (query^.insertValues) 

-- | Build an UPDATE query.
--instance (
--      Parser a Assignment
--    , Parser a Where
--    , Parser a SqlValue
--    ) => DefaultParser a Update where
--    toDefaultSqlString driver query =
--              "UPDATE " ++ (query^.updateTable.tableName)
--         ++ " SET" ++ assignments
--         ++ (makeWhere driver $ query^.updateWherePart)
--         where
--             assignments = getPatternFromList " " "" ", " $ map (toSqlString driver) $ query^.updateAssignments

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

-- | Build a table constraint.
--instance (
--      Parser a ConstraintTiming
--    , Parser a TableConstraintType) => DefaultParser a TableConstraint where
--    toDefaultSqlString driver table =
--             makeName (table^.tableConstraintName)
--        ++ toSqlString driver (table^.tableConstraintConstraint)
--        ++ makeTiming driver (table^.tableConstraintTiming)
--        where
--            -- TODO: check if constraints name can be quoted.
--            makeName (Just name) = "CONSTRAINT " ++ quoteSql driver name ++ " "
--            makeName Nothing = ""
--            makeTiming driver (Just timing) = " " ++ toSqlString driver timing
--            makeTiming driver Nothing = ""

-- Build a table constraint type.
--instance (
--      Parser a Column
--    , Parser a Condition
--    , Parser a ForeignKeyClause) => DefaultParser a TableConstraintType where
--    toDefaultSqlString driver (TableConstraintCheck condition) =
--             "Check" ++ toSqlString driver condition
--    
--    toDefaultSqlString driver (ForeignKey cols clause) =
--             "FOREIGN KEY"
--        ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) cols)
--        ++ toSqlString driver clause
--    
--    toDefaultSqlString driver (TableConstraintPrimaryKey cols) =
--             "PRIMARY KEY" ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) cols)
--        
--    toDefaultSqlString driver (TableConstraintUnique cols) =
--             "UNIQUE" ++ getPatternFromList " (" ")" ", " (map (toSqlString driver) cols)

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
--makeColumn
--    :: (Driver a, Parser a SqlDataType, Parser a ColConstraint) =>
--       a -> Column -> [Char]
--makeColumn driver column =
--       quoteSql driver (column^.colName)
--    ++ makeDataType driver (column^.colDataType)
--    ++ makeColumnConstraints driver (column^.colConstraints)

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
--makeDataType :: (Driver a, Parser a b) => a -> Maybe b -> [Char]
--makeDataType driver (Just dataType) = " " ++ toSqlString driver dataType
--makeDataType driver Nothing = ""