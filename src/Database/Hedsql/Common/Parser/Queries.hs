{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Database/Hedsql/Common/Parser/Query.hs
Description : Implementation of the SQL query parsers.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL statement parsers as well as the queries parser.
-}
module Database.Hedsql.Common.Parser.Queries
    ( StmtParser(StmtParser)
    , parseCombined
    , parseCreateTable
    , parseCreateView
    , parseDelete
    , parseDropTable
    , parseDropView
    , parseInsert
    , parseSelect
    , parseStatement
    , parseUpdate
    
    , parseCombinedFunc
    , parseDeleteFunc
    , parseDropTableFunc
    , parseDropViewFunc
    , parseInsertFunc
    , parseSelectFunc
    , parseStmtFunc
    , parseUpdateFunc
    
    , QueryParser(QueryParser)
    , parseAssgnmt
    , parseCol
    , parseColRef
    , parseColRefDef
    , parseCondition
    , parseFunc
    , parseFuncBool
    , parseExpr
    , parseFrom
    , parseJoin
    , parseGroupBy
    , parseHaving
    , parseOrderBy
    , parseSortNull
    , parseSortRef
    , parseSortOrder
    , parseTableName
    , parseTableRef
    , parseTableRefAs
    , parseValue 
    , parseWhere
    , quoteElem
    , quoteVal
    
    , parseAssgnmtFunc
    , parseColFunc
    , parseColRefFunc
    , parseColRefDefFunc
    , parseConditionFunc
    , parseExprFunc
    , parseFromFunc
    , parseFuncBoolFunc
    , parseFuncFunc
    , parseJoinFunc
    , parseGroupByFunc
    , parseHavingFunc
    , parseOrderByFunc
    , parseSortNullFunc
    , parseSortRefFunc
    , parseSortOrderFunc
    , parseTableNameFunc
    , parseTableRefFunc
    , parseTableRefAsFunc
    , parseValueFunc
    , parseWhereFunc
    
    , JoinParser(JoinParser)
    , parseJoinClause
    , parseJoinTCol
    , parseJoinTTable
    
    , parseJoinClauseFunc
    , parseJoinTColFunc
    , parseJoinTTableFunc
    ) where

import Database.Hedsql.Common.DataStructure
import Database.Hedsql.Common.Parser.Interface()

import Control.Lens
import Data.List (intercalate)
import Data.Maybe


-- Definition of the parsers interfaces.

-- | Interface of the statements parser.
data StmtParser a = StmtParser
    { _parseCombined    :: CombinedQuery a -> String
    , _parseCreateTable :: Table         a -> String
    , _parseCreateView  :: CreateView    a -> String
    , _parseDelete      :: Delete        a -> String
    , _parseDropTable   :: DropTable     a -> String
    , _parseDropView    :: DropView      a -> String
    , _parseInsert      :: Insert        a -> String
    , _parseSelect      :: Select        a -> String
    , _parseStatement   :: Statement     a -> String
    , _parseUpdate      :: Update        a -> String
    }

makeLenses ''StmtParser

{-|
Interface of the query parser.
-}
data QueryParser a = QueryParser
    { _parseAssgnmt    :: Assignment a    -> String
    , _parseCol        :: Column a        -> String
    , _parseColRef     :: ColRef a        -> String
    , _parseColRefDef  :: ColRef a        -> String
    , _parseCondition  :: Condition a     -> String
    , _parseFunc       :: Function a      -> String
    , _parseFuncBool   :: FuncBool a      -> String
    , _parseExpr       :: Expression a    -> String
    , _parseFrom       :: From a          -> String
    , _parseJoin       :: Join a          -> String
    , _parseGroupBy    :: GroupBy a       -> String
    , _parseHaving     :: Having a        -> String
    , _parseOrderBy    :: OrderBy a       -> String
    , _parseSortNull   :: SortNulls a     -> String
    , _parseSortRef    :: SortRef a       -> String
    , _parseSortOrder  :: SortOrder a     -> String
    , _parseTableName  :: Table a         -> String
    , _parseTableRef   :: TableRef a      -> String
    , _parseTableRefAs :: TableRefAs a    -> String
    , _parseValue      :: SqlValue a      -> String
    , _parseWhere      :: Where a         -> String
    
    -- Helper functions.
    , _quoteElem :: String -> String
    , _quoteVal  :: String -> String
    }

makeLenses ''QueryParser

-- | Parse the joins of a SELECT query in a FROM clause.
data JoinParser a = JoinParser
    { _parseJoinClause :: JoinClause a    -> String
    , _parseJoinTCol   :: JoinTypeCol a   -> String
    , _parseJoinTTable :: JoinTypeTable a -> String
    }

makeLenses ''JoinParser

-- Query parser.

-- Private functions.

{-|
If the provided boolean is true, return the provided string.
If the provided boolean is false, return an empty string.
-}
parseIf :: Bool -> String -> String
parseIf True  x = x
parseIf False _ = ""

{-|
Apply a parsing function to a maybe.
If Just, returns the result of the parse function preceeded by a space.
If Nothing, returns an empty string.
-}
parseMaybe :: (a -> String) -> Maybe a -> String
parseMaybe f = maybe "" ((++) " " . f)
  
-- Implementation of the functions.

{-|
Parse the assignment of an UPDATE statement.

Note: this function is located in the Query Parser because it is the only
one specific to the UPDATE statement. Thus, a dedicated UPDATE parser
for this only purpose wouldn't make a lot of sense.
-}
parseAssgnmtFunc :: QueryParser a -> Assignment a -> String
parseAssgnmtFunc parser assignment =
    concat
        [ (parser^.parseCol)  (assignment^.assignmentCol)
        , " = "
        , (parser^.parseExpr) (assignment^.assignmentVal)
        ]

{-|
Parse the name of a column.
If the column has a table name defined,
the qualified name ("tableName.columnName") will be returned.
Else, the name of the column itself will be returned.
-}
parseColFunc :: QueryParser a -> Column a -> String
parseColFunc parser (Column name _ _ table) = 
       parseT table ++ (parser^.quoteElem) name
    where
        parseT (Just tb) = (parser^.parseTableName) tb ++ "."
        parseT  Nothing = ""
        
-- | Define a column reference using its alias if defined.
parseColRefFunc :: QueryParser a -> ColRef a -> String
parseColRefFunc parser colRef =
    case colRef^.colRefLabel of
        Just name -> parser^.quoteElem $ name
        Nothing   -> (parser^.parseExpr) (colRef^.colRefExpr)
        
-- | Define a column reference including its alias definition if specified.
parseColRefDefFunc :: QueryParser a -> ColRef a -> String
parseColRefDefFunc parser colRef =
       (parser^.parseExpr) (colRef^.colRefExpr)
    ++  parseMaybe (" AS" ++) (fmap (parser^.quoteElem) (colRef^.colRefLabel))

-- | Parse a combined query such as "UNION", "EXCEPT", etc.
parseCombinedFunc :: StmtParser a -> CombinedQuery a -> String 
parseCombinedFunc parser query =
    case query of
        (CombinedQuerySingle select)      -> (parser^.parseSelect) select
        (CombinedQueryExcept combs)       -> combine combs "EXCEPT"
        (CombinedQueryExceptAll combs)    -> combine combs "EXCEPT ALL"
        (CombinedQueryIntersect combs)    -> combine combs "INTERSECT"
        (CombinedQueryIntersectAll combs) -> combine combs "INTERSECT ALL"
        (CombinedQueryUnion combs)        -> combine combs "UNION"
        (CombinedQueryUnionAll combs)     -> combine combs "UNION ALL"
    where
        combine cs typ =
            concat ["(", intercalate (" " ++ typ ++ " ") $ combineds cs, ")"]
        combineds = map (parser^.parseCombined)
        
-- | Parse a condition.
parseConditionFunc :: QueryParser a -> Condition a -> String
parseConditionFunc parser condition =
    case condition of
        FuncCond func  -> parser^.parseFuncBool $ func
        And conds      -> pCond "AND" conds
        Or  conds      -> pCond "OR"  conds
    where
        cs = map (parser^.parseCondition)
        pCond name conds = intercalate (" " ++ name ++ " ") $ cs conds
            
-- | Parse a DELETE statement.
parseDeleteFunc :: QueryParser a -> Delete a -> String
parseDeleteFunc parser statement =
       "DELETE "
    ++            (parser^.parseTableName) (statement^.deleteTable) 
    ++ parseMaybe (parser^.parseWhere)     (statement^.deleteWhere)

-- | Parse a DROP TABLE statement.
parseDropTableFunc :: QueryParser a -> DropTable a -> String
parseDropTableFunc parser statement =
        "DROP TABLE "
    ++  parseIf (statement^.dropTableIfExistsParam) "IF EXISTS "
    ++  (parser^.parseTableName) (statement^.dropTableTable)

-- | Parse a DROP VIEW statement.
parseDropViewFunc :: QueryParser a -> DropView a -> String
parseDropViewFunc parser statement =
    "Drop View " ++ (parser^.quoteElem $ statement^.dropViewName)

-- | Parse a SQL expression.
parseExprFunc :: QueryParser a -> StmtParser a -> Expression a -> String
parseExprFunc parser stmtParser expr =
    case expr of
        ColExpr col        -> parser^.parseCol $ col
        SelectExpr select  -> "(" ++ (stmtParser^.parseSelect) select ++ ")"   
        FuncExpr func      -> parser^.parseFunc $ func
        ValueExpr val      -> parser^.parseValue $ val
        ValueExprs vals    -> concat
                              ["("
                              , intercalate ", " $ map (parser^.parseValue) vals
                              , ")"
                              ]

-- | Parse a FROM clause.
parseFromFunc :: QueryParser a -> From a -> String
parseFromFunc parser (From tableReferences) =
    foldl (++) "FROM " $ map (parser^.parseTableRef) tableReferences
       
-- | Parse a function returning a boolean value.
parseFuncBoolFunc :: QueryParser a -> FuncBool a -> String
parseFuncBoolFunc parser funcBool =
    case funcBool of
        Between    ref lower higher -> parseBetweens True ref lower higher
        Equal             ref1 ref2 -> parseInfix "=" ref1 ref2
        Exists            colRef    -> concat
                                            ["(EXISTS "
                                            , parser^.parseColRef $ colRef
                                            , ")"
                                            ]
        GreaterThan       ref1 ref2 -> parseInfix ">" ref1 ref2
        GreaterThanOrEqTo ref1 ref2 -> parseInfix ">=" ref1 ref2
        In                ref1 ref2 -> parseInfix "IN" ref1 ref2
        IsDistinctFrom    ref1 ref2 -> parseInfix "IS DISTINCT FROM" ref1 ref2
        IsFalse           expr      -> parseIs expr "FALSE"
        IsNotDistinctFrom ref1 ref2 -> parseInfix
                                            "IS NOT DISTINCT FROM"
                                            ref1
                                            ref2
        IsNotFalse        expr      -> parseIs expr "NOT FALSE"
        IsNotNull         expr      -> parseIs expr "NOT NULL"
        IsNotTrue         expr      -> parseIs expr "NOT TRUE"
        IsNotUnknown      expr      -> parseIs expr "NOT UNKNOWN"
        IsNull            expr      -> parseIs expr "NULL"
        IsTrue            expr      -> parseIs expr "TRUE"
        IsUnknown         expr      -> parseIs expr "UNKNOWN"
        Like              ref1 ref2 -> parseInfix "LIKE" ref1 ref2
        NotBetween ref lower higher -> parseBetweens False ref lower higher
        NotEqual          ref1 ref2 -> parseInfix "<>" ref1 ref2
        NotIn             ref1 ref2 -> parseInfix "NOT IN" ref1 ref2
        SmallerThan       ref1 ref2 -> parseInfix "<" ref1 ref2
        SmallerThanOrEqTo ref1 ref2 -> parseInfix "<=" ref1 ref2
    where
        parseBetweens func colRef lower higher =
            concat
                [parser^.parseColRef $ colRef
                , " "
                , if func then "" else "NOT"
                , " BETWEEN "
                , parser^.parseColRef $ lower
                , " AND "
                , parser^.parseColRef $ higher
                ]
        parseInfix name colRef1 colRef2 =
            concat
                [parser^.parseColRef $ colRef1
                , " "
                , name
                , " "
                , parser^.parseColRef $ colRef2
                ]
        parseIs colRef text =
            concat
                [parser^.parseColRef $ colRef
                , "IS "
                , text
                ]

-- | Parse a function.
parseFuncFunc :: QueryParser a -> Function a -> String
parseFuncFunc parser func =
    case func of
        -- Operators.
        Add           left right -> parseInfix "+" left right
        BitAnd        left right -> parseInfix "&" left right
        BitOr         left right -> parseInfix "|" left right
        BitShiftLeft  left right -> parseInfix "<<" left right
        BitShiftRight left right -> parseInfix ">>" left right
        Divide        left right -> parseInfix "/" left right
        Modulo        left right -> parseInfix "%" left right
        Multiply      left right -> parseInfix "*" left right
        Substract     left right -> parseInfix "-" left right 
    
        -- Functions.
        Count       expr -> makeExpr "COUNT" expr
        CurrentDate      -> "CURRENT_DATE"
        Joker            -> "*"
        Max         expr -> makeExpr "MAX" expr
        Min         expr -> makeExpr "MIN" expr
        Random           -> "random()"
        Sum         expr -> makeExpr "SUM" expr
        
        -- MariaDB functions.
        CalcFoundRows -> error
               "SQL_CALC_FOUND_ROWS is specific to MariaDB."
            ++ "Use the MariaDB parser."
        FoundRows     -> error
              "FOUND_ROWS is specific to MariaDB. Use the MariaDB parser."
    where
        makeExpr string expression =
            concat [string, "(", parser^.parseExpr $ expression, ")"]
        parseInfix name ref1 ref2 =
            concat
                [ "("
                , parser^.parseColRef $ ref1
                , " "
                , name
                , " "
                , parser^.parseColRef $ ref2
                , ")"
                ]

-- | Parse a GROUP BY clause.
parseGroupByFunc :: QueryParser a -> GroupBy a -> String
parseGroupByFunc parser (GroupBy colRefs having) =
    concat
        [ "GROUP BY "
        , intercalate ", " $ map (parser^.parseColRef) colRefs
        , parseMaybe (parser^.parseHaving) having
        ] 

-- | Parse a HAVING clause.
parseHavingFunc :: QueryParser a -> Having a -> String
parseHavingFunc parser (Having c) = "HAVING " ++ (parser^.parseCondition) c

parseInsertFunc :: QueryParser a -> Insert a -> String
parseInsertFunc parser insert =
    concat $ catMaybes
        [ Just "INSERT INTO "
        , Just $ parser^.quoteElem $ insert^.insertTable.tableName
        , fmap parseCols (insert^.insertColumns)
        , Just " VALUES "
        , Just $ intercalate ", " $ map parseParam (insert^.insertValues) 
        ]
    where
        parseCols cols =
            concat [" (", intercalate ", " $ map (parser^.parseCol) cols, ")"]
        parseParam ps =
            concat ["(", intercalate ", " $ map (parser^.parseValue) ps, ")"]

-- | Parse joins.
parseJoinFunc :: QueryParser a -> JoinParser a -> Join a -> String
parseJoinFunc queryParser parser join =
    case join of
        JoinColumn joinType tableRef1 tableRef2 clause ->
            pJoin (joinCol joinType) tableRef1 tableRef2 (Just clause)
            
        
        JoinTable joinType tableRef1 tableRef2 ->
            pJoin (joinTable joinType) tableRef1 tableRef2 Nothing
    where
        joinCol   = parser^.parseJoinTCol
        joinTable = parser^.parseJoinTTable
        
        -- Common part between column and table joins.
        pJoin joinType tableRef1 tableRef2 clause = concat
            [
              queryParser^.parseTableRef $ tableRef1
            , getAlias tableRef1
            , " " ++ joinType ++ " "
            , queryParser^.parseTableRef $ tableRef2
            , getAlias tableRef2
            , parseMaybe (parser^.parseJoinClause) clause
            ]
        
        -- Alias clause for the table references inside the join.
        getAlias ref =
            parseMaybe (queryParser^.parseTableRefAs) (getTableRefAlias ref)
    
-- Joins parser functions.

-- | Parse the ON or USING clause of a JOIN.
parseJoinClauseFunc :: QueryParser a -> JoinClause a -> String
parseJoinClauseFunc parser jClause =
    case jClause of
        JoinClauseOn predicate -> "ON " ++ (parser^.parseCondition) predicate
        JoinClauseUsing cols -> concat
                                [ "USING ("
                                , intercalate ", " $ map (parser^.parseCol) cols
                                , ")"
                                ]

-- | Parser a join on a column.
parseJoinTColFunc :: JoinTypeCol a -> String
parseJoinTColFunc join =
    case join of
        FullJoin  -> "FULL JOIN"
        LeftJoin  -> "LEFT JOIN"
        InnerJoin -> "INNER JOIN"
        RightJoin -> "RIGHT JOIN"

-- | Parser a join on a table.
parseJoinTTableFunc :: JoinTypeTable a -> String
parseJoinTTableFunc joinType =
    case joinType of
        CrossJoin        -> "CROSS JOIN"
        NaturalFullJoin  -> "NATURAL FULL JOIN"
        NaturalLeftJoin  -> "NATURAL LEFT JOIN"
        NaturalInnerJoin -> "NATURAL INNER JOIN"
        NaturalRightJoin -> "NATURAL RIGHT JOIN"
        
-- | Parse an ORDER BY clause.
parseOrderByFunc :: QueryParser a -> OrderBy a -> String
parseOrderByFunc parser clause =
    concat
        [ "ORDER BY "
        , intercalate ", " sortRefsParsed
        , parseMaybe show (clause^.partOrderByLimit)
        , parseMaybe show (clause^.partOrderByOffset)
        ]
    where
        sortRefsParsed = map (parser^.parseSortRef) (clause^.partOrderByColumns)

-- | Parse a SELECT query.
parseSelectFunc :: QueryParser a -> Select a -> String
parseSelectFunc parser select =
      concat $ catMaybes
        [ Just       "SELECT "
        , fmap        parseDistinct         (select^.selectType)
        , Just $      parseColRefs          (select^.selectColRef)
        , parseM (parser^.parseFrom)    (select^.fromClause)
        , parseM (parser^.parseWhere)   (select^.whereClause)
        , parseM (parser^.parseGroupBy) (select^.groupByClause)
        , parseM (parser^.parseOrderBy) (select^.orderByClause)
        ]
    where
        parseColRefs cRef = intercalate ", " $ map (parser^.parseColRef) cRef
        
        parseDistinct  All               = "All"
        parseDistinct  Distinct          = "DISTINCT "
        parseDistinct (DistinctOn exprs) =
            concat
                [ "DISTINCT ON ("
                , intercalate ", " $ map (parser^.parseExpr) exprs
                , ") "
                ]
        
        parseM f = fmap (\x -> " " ++ f x)

-- | Parse the NULLS FIRST or NULLS LAST of a the sorting clause.
parseSortNullFunc :: SortNulls a -> String
parseSortNullFunc NullsFirst = "NULLS FIRST"
parseSortNullFunc NullsLast  = "NULLS LAST"

-- | Parse the ASC or DESC clauses.
parseSortOrderFunc :: SortOrder a -> String
parseSortOrderFunc Asc  = "ASC"
parseSortOrderFunc Desc = "DESC"

-- | Parse a sort reference.
parseSortRefFunc :: QueryParser a -> SortRef a -> String
parseSortRefFunc parser sortRef =
                      (parser^.parseColRef)       (sortRef^.sortRefCol)
        ++ parseMaybe (parser^.parseSortOrder)    (sortRef^.sortRefOrder)
        ++ parseMaybe (parser^.parseSortNull)     (sortRef^.sortRefNulls)

-- | Parse a SQL statement.
parseStmtFunc :: StmtParser a -> Statement a -> String
parseStmtFunc parser stmt =
    case stmt of
        CreateTableStmt s  -> parser^.parseCreateTable $ s
        CreateViewStmt  s  -> parser^.parseCreateView  $ s
        DeleteStmt      s  -> parser^.parseDelete      $ s
        DropTableStmt   s  -> parser^.parseDropTable   $ s
        DropViewStmt    s  -> parser^.parseDropView    $ s
        InsertStmt      s  -> parser^.parseInsert      $ s
        SelectStmt      s  -> parser^.parseSelect      $ s
        UpdateStmt      s  -> parser^.parseUpdate      $ s
        Statements      xs -> concatMap
                                  (\x -> (parser^.parseStatement $ x) ++ "; ")
                                   xs
        
{-|
Parse the name of a table.
-}
parseTableNameFunc :: QueryParser a -> Table a -> String
parseTableNameFunc parser table = parser^.quoteElem $ table^.tableName

-- | Parse a table reference for the use in a FROM clause.
parseTableRefFunc :: StmtParser a -> QueryParser a -> TableRef a -> String
parseTableRefFunc stmtP parser join =
    case join of
        TableJoinRef    ref    alias -> concat [ maybe "" (const "(") alias
                                               , parser^.parseJoin $ ref
                                               , maybe "" (const ")") alias
                                               , parseMaybe pAlias alias
                                               ]
        TableTableRef   table  alias ->   (parser^.parseTableName) table
                                        ++ parseMaybe pAlias alias
        SelectTableRef  select alias -> concat [ "("
                                               , stmtP^.parseSelect $ select
                                               , ")"
                                               , pAlias alias
                                               ]
        LateralTableRef select alias -> concat [ "LATERAL ("
                                               , stmtP^.parseSelect $ select
                                               , ")"
                                               , pAlias alias
                                               ]
    where
        pAlias alias = " " ++ (parser^.parseTableRefAs) alias

-- | Parse a table alias.
-- TODO: create a dedicated one for PostgreSQL for the columns aliaises
parseTableRefAsFunc :: QueryParser a -> TableRefAs a -> String
parseTableRefAsFunc parser alias =
    "AS " ++ (parser^.quoteElem) (alias^.tableRefAliasName)

-- | Parse an input values.
parseValueFunc :: QueryParser a -> SqlValue a -> String
parseValueFunc _       SqlValueDefault         = "DEFAULT"
parseValueFunc _      (SqlValueInt int)        = show int
parseValueFunc _       SqlValueNull            = "NULL"
parseValueFunc parser (SqlValueString string)  = parser^.quoteVal $ string
parseValueFunc _       Placeholder             = "?"

-- | Parse a WHERE clause.  
parseWhereFunc :: QueryParser a -> Where a -> String
parseWhereFunc parser (Where condition) =
    "WHERE " ++ (parser^.parseCondition) condition

-- | Parse an UPDATE statement.
parseUpdateFunc :: QueryParser a -> Update a -> String    
parseUpdateFunc parser update = 
    concat $ catMaybes
        [ Just   "UPDATE "
        , Just $ parser^.quoteElem $ update^.updateTable.tableName
        , Just   " SET "
        , Just $ intercalate ", " $ map (parser^.parseAssgnmt) assignments
        , fmap (parser^.parseWhere) (update^.updateWherePart)
        ]
    where
        assignments = update^.updateAssignments