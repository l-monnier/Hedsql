{-# LANGUAGE GADTs           #-}
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
    (
      -- * Statement parser
      
      -- ** Interface
      StmtParser(StmtParser)
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
      
      -- ** Functions implementation
    , parseCombinedFunc
    , parseDeleteFunc
    , parseDropTableFunc
    , parseDropViewFunc
    , parseInsertFunc
    , parseSelectFunc
    , parseStmtFunc
    , parseUpdateFunc
    
      -- * Query parser
      
      -- ** Interface
    , QueryParser(QueryParser)
    , parseAssgnmt
    , parseCol
    , parseColRef
    , parseColRefDef
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
      -- *** Helper functions
    , quoteElem
    , quoteVal
    
      -- ** Functions implementation
    , parseAssgnmtFunc
    , parseColFunc
    , parseColRefFunc
    , parseColRefDefFunc
    , parseExprFunc
    , parseFromFunc
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
    
      -- * Join Parser
      -- ** Interface
    , JoinParser(JoinParser)
    , parseJoinClause
    , parseJoinTCol
    , parseJoinTTable
    
      -- ** Functions implementation
    , parseJoinClauseFunc
    , parseJoinTColFunc
    , parseJoinTTableFunc
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.DataStructure
import Database.Hedsql.Common.Parser.Interface()

import Control.Lens
import Data.List (intercalate)
import Data.Maybe

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All interface
----------------------------------------

-- All interfaces are grouped because of the template haskell lenses
-- which need to be generated.

-- | Interface of the statements parser.
data StmtParser a = StmtParser
    { _parseCombined    :: CombinedQuery a -> String
    , _parseCreateTable :: Table         a -> String
    , _parseCreateView  :: CreateView    a -> String
    , _parseDelete      :: Delete        a -> String
    , _parseDropTable   :: DropTable     a -> String
    , _parseDropView    :: DropView      a -> String
    , _parseInsert      :: Insert        a -> String
    , _parseSelect      :: SelectWrap    a -> String
    , _parseStatement   :: Statement     a -> String
    , _parseUpdate      :: Update        a -> String
    }

makeLenses ''StmtParser

{-|
Interface of the query parser.
-}
data QueryParser a = QueryParser
    { _parseAssgnmt    :: Assignment a -> String
    , _parseCol        :: ColWrap    a -> String
    , _parseColRef     :: ColRefWrap a -> String
    , _parseColRefDef  :: ColRefWrap a -> String
    , _parseExpr       :: ExprWrap   a -> String
    , _parseFrom       :: From       a -> String
    , _parseJoin       :: Join       a -> String
    , _parseGroupBy    :: GroupBy    a -> String
    , _parseHaving     :: Having     a -> String
    , _parseOrderBy    :: OrderBy    a -> String
    , _parseSortNull   :: SortNulls  a -> String
    , _parseSortRef    :: SortRef    a -> String
    , _parseSortOrder  :: SortOrder  a -> String
    , _parseTableName  :: Table      a -> String
    , _parseTableRef   :: TableRef   a -> String
    , _parseTableRefAs :: TableRefAs a -> String
    , _parseValue      :: ValueWrap  a -> String
    , _parseWhere      :: Where      a -> String
    
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

----------------------------------------
-- Statement parser
----------------------------------------

--------------------
-- Functions implementation
--------------------

-- | Parse a combined query such as "UNION", "EXCEPT", etc.
parseCombinedFunc :: StmtParser a -> CombinedQuery a -> String 
parseCombinedFunc parser query =
    case query of
        Single       select -> (parser^.parseSelect) select
        Except       combs  -> combine combs "EXCEPT"
        ExceptAll    combs  -> combine combs "EXCEPT ALL"
        Intersect    combs  -> combine combs "INTERSECT"
        IntersectAll combs  -> combine combs "INTERSECT ALL"
        Union        combs  -> combine combs "UNION"
        UnionAll     combs  -> combine combs "UNION ALL"
    where
        combine cs typ =
            concat ["(", intercalate (" " ++ typ ++ " ") $ combineds cs, ")"]
        combineds = map (parser^.parseCombined)

-- | Parse a DELETE statement.
parseDeleteFunc :: QueryParser a -> Delete a -> String
parseDeleteFunc parser statement =
       "DELETE FROM "
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

parseInsertFunc :: QueryParser a -> Insert a -> String
parseInsertFunc parser insert =
    concat $ catMaybes
        [ Just "INSERT INTO "
        , Just $ parser^.quoteElem $ insert^.insertTable.tableName
        , fmap parseCols (insert^.insertCols)
        , Just " VALUES "
        , Just $ intercalate ", " $ map parseParam (insert^.insertValues) 
        ]
    where
        parseCols cols =
            concat [" (", intercalate ", " $ map (parser^.parseCol) cols, ")"]
        parseParam ps =
            concat ["(", intercalate ", " $ map (parser^.parseValue) ps, ")"]

-- | Parse a SELECT query.
parseSelectFunc :: QueryParser a -> SelectWrap a -> String
parseSelectFunc parser (SelectWrap select) =
      concat $ catMaybes
        [ Just   "SELECT "
        , fmap   parseDistinct          (body^.selectType)
        , Just $ parseColRefs           (getSelectColRefs select)
        , parseM (parser^.parseFrom)    (body^.fromClause)
        , parseM (parser^.parseWhere)   (body^.whereClause)
        , parseM (parser^.parseGroupBy) (body^.groupByClause)
        , parseM (parser^.parseOrderBy) (body^.orderByClause)
        ]
    where
        body = select^.selectBody
        
        parseColRefs cRef = intercalate ", " $ map (parser^.parseColRefDef) cRef
        
        parseDistinct  All              = "All"
        parseDistinct  Distinct         = "DISTINCT "
        parseDistinct (DistinctOn refs) =
            concat
                [ "DISTINCT ON ("
                , intercalate ", " $ map (parser^.parseColRef) refs
                , ") "
                ]
        
        parseM f = fmap $ (++) " " . f

-- | Parse a SQL statement.
parseStmtFunc :: StmtParser a -> Statement a -> String
parseStmtFunc parser stmt =
    case stmt of
        CombinedQueryStmt s  -> parser^.parseCombined    $ s
        CreateTableStmt   s  -> parser^.parseCreateTable $ s
        CreateViewStmt    s  -> parser^.parseCreateView  $ s
        DeleteStmt        s  -> parser^.parseDelete      $ s
        DropTableStmt     s  -> parser^.parseDropTable   $ s
        DropViewStmt      s  -> parser^.parseDropView    $ s
        InsertStmt        s  -> parser^.parseInsert      $ s
        SelectStmt        s  -> parser^.parseSelect      $ s
        UpdateStmt        s  -> parser^.parseUpdate      $ s
        Statements        xs -> xs >>= flip (++) "; " . (parser^.parseStatement)
                                  

-- | Parse an UPDATE statement.
parseUpdateFunc :: QueryParser a -> Update a -> String    
parseUpdateFunc parser update = 
    concat $ catMaybes
        [ Just   "UPDATE "
        , Just $ parser^.quoteElem $ update^.updateTable.tableName
        , Just   " SET "
        , Just $ intercalate ", " $ map (parser^.parseAssgnmt) assignments
        , fmap ((++) " " . (parser^.parseWhere)) (update^.updateWherePart)
        ]
    where
        assignments = update^.updateAssignments

----------------------------------------
-- Query parser
----------------------------------------

--------------------
-- Functions implementation
--------------------

{-|
Parse the assignment of an UPDATE statement.

Note: this function is located in the Query Parser because it is the only
one specific to the UPDATE statement. Thus, a dedicated UPDATE parser
for this only purpose wouldn't make a lot of sense.
-}
parseAssgnmtFunc :: QueryParser a -> Assignment a -> String
parseAssgnmtFunc parser (Assignment col val) =
    concat
        [ (parser^.parseCol) $ ColWrap col
        , " = "
        , (parser^.parseExpr) $ ExprWrap val
        ]

-- | Parse the name of a column.
parseColFunc :: QueryParser a -> ColWrap a -> String
parseColFunc parser (ColWrap col) = (parser^.quoteElem) (col^.colName)
        
{-|
Define a column reference using its alias if defined.

If the column reference is a column and belongs to a specified table reference,
then a qualified name will be returned (for example: "Table1"."col1").
-}
parseColRefFunc :: QueryParser a -> ColRefWrap a -> String
parseColRefFunc parser (ColRefWrap colRef) =
        maybe ifNothing makeLabel (colRef^.colRefLabel)
    where
        ifNothing = (parser^.parseExpr) $ ExprWrap refExpr
        
        makeLabel refLabel = tableRef refExpr ++ quote refLabel

        tableRef (ColExpr (ColDef _ (Just l))) =
            quote (getTableRefName l) ++ "."
        tableRef _                    = ""
        
        quote = parser^.quoteElem
        
        refExpr = colRef^.colRefExpr
        
-- | Define a column reference including its alias definition if specified.
parseColRefDefFunc :: QueryParser a -> ColRefWrap a -> String
parseColRefDefFunc parser (ColRefWrap colRef) =
       (parser^.parseExpr) (ExprWrap $ colRef^.colRefExpr)
    ++ maybe "" ((++) " AS " . (parser^.quoteElem)) (colRef^.colRefLabel)

-- | Parse a SQL expression.
parseExprFunc :: QueryParser a -> StmtParser a -> ExprWrap a -> String
parseExprFunc parser stmtParser (ExprWrap expr) =
    case expr of
        ColExpr (ColDef col l) ->
            concat
                [ maybe
                      ""
                      (flip (++) "." . (parser^.quoteElem) . getTableRefName)
                      l
                , parser^.parseCol $ ColWrap col
                ]
        And conds      -> pCond "AND" $ map ExprWrap conds
        Or  conds      -> pCond "OR" $ map ExprWrap conds    
        SelectExpr select ->
            concat ["(", stmtParser^.parseSelect $ SelectWrap select, ")"]
            
        Value  val    ->
            parser^.parseValue $ ValueWrap val
        Values vals   ->
            concat
                ["("
                , intercalate ", " $ map ((parser^.parseValue) . ValueWrap) vals
                , ")"
                ]
        
        -- Boolean Functions.
        Between ref lower higher ->
            parseBetweens
                True
                (ColRefWrap ref)
                (ColRefWrap lower)
                (ColRefWrap higher)
        Equal ref1 ref2 ->
            parseInfix "=" (ColRefWrap ref1) (ColRefWrap ref2)
        Exists colRef ->
            "EXISTS " ++ (parser^.parseColRef) (ColRefWrap colRef)
        GreaterThan       ref1 ref2 ->
            parseInfix ">" (ColRefWrap ref1) (ColRefWrap ref2)
        GreaterThanOrEqTo ref1 ref2 ->
            parseInfix ">=" (ColRefWrap ref1) (ColRefWrap ref2)
        In                ref1 ref2 ->
            parseInfix "IN" (ColRefWrap ref1) (ColRefWrap ref2)
        IsDistinctFrom    ref1 ref2 ->
            parseInfix "IS DISTINCT FROM" (ColRefWrap ref1) (ColRefWrap ref2)
        IsFalse expr -> parseIs (ColRefWrap expr) "FALSE"
        IsNotDistinctFrom ref1 ref2 -> parseInfix
                                            "IS NOT DISTINCT FROM"
                                            (ColRefWrap ref1)
                                            (ColRefWrap ref2)
        IsNotFalse        expr      -> parseIs (ColRefWrap expr) "NOT FALSE"
        IsNotNull         expr      -> parseIs (ColRefWrap expr) "NOT NULL"
        IsNotTrue         expr      -> parseIs (ColRefWrap expr) "NOT TRUE"
        IsNotUnknown      expr      -> parseIs (ColRefWrap expr) "NOT UNKNOWN"
        IsNull            expr      -> parseIs (ColRefWrap expr) "NULL"
        IsTrue            expr      -> parseIs (ColRefWrap expr) "TRUE"
        IsUnknown         expr      -> parseIs (ColRefWrap expr) "UNKNOWN"
        Like              ref1 ref2 ->
            parseInfix "LIKE" (ColRefWrap ref1) (ColRefWrap ref2)
        NotBetween ref lower higher ->
            parseBetweens
                False
                (ColRefWrap ref)
                (ColRefWrap lower)
                (ColRefWrap higher)
        NotEqual ref1 ref2 ->
            parseInfix "<>" (ColRefWrap ref1) (ColRefWrap ref2)
        NotIn ref1 ref2 ->
            parseInfix "NOT IN" (ColRefWrap ref1) (ColRefWrap ref2)
        SmallerThan ref1 ref2 ->
            parseInfix "<" (ColRefWrap ref1) (ColRefWrap ref2)
        SmallerThanOrEqTo ref1 ref2 ->
            parseInfix "<=" (ColRefWrap ref1) (ColRefWrap ref2)
        
        -- Operators.
        Add left right ->
            parseInfix "+" (ColRefWrap left) (ColRefWrap right)
        BitAnd left right -> parseInfix "&" (ColRefWrap left) (ColRefWrap right)
        BitOr left right -> parseInfix "|" (ColRefWrap left) (ColRefWrap right)
        BitShiftLeft  left right ->
            parseInfix "<<" (ColRefWrap left) (ColRefWrap right)
        BitShiftRight left right ->
            parseInfix ">>" (ColRefWrap left) (ColRefWrap right)
        Divide left right ->
            parseInfix "/" (ColRefWrap left) (ColRefWrap right)
        Modulo left right ->
            parseInfix "%" (ColRefWrap left) (ColRefWrap right)
        Multiply left right ->
            parseInfix "*" (ColRefWrap left) (ColRefWrap right)
        Substract left right ->
            parseInfix "-" (ColRefWrap left) (ColRefWrap right) 
    
        -- Functions.
        Count       expr -> makeExpr "COUNT" (ColRefWrap expr)
        CurrentDate      -> "CURRENT_DATE"
        Joker            -> "*"
        Max         expr -> makeExpr "MAX" (ColRefWrap expr)
        Min         expr -> makeExpr "MIN" (ColRefWrap expr)
        Random           -> "random()"
        Sum         expr -> makeExpr "SUM" (ColRefWrap expr)
        
        -- MariaDB functions.
        CalcFoundRows -> error
               "SQL_CALC_FOUND_ROWS is specific to MariaDB."
            ++ "Use the MariaDB parser."
        FoundRows     -> error
              "FOUND_ROWS is specific to MariaDB. Use the MariaDB parser."
    
    where
        pCond name conds =
            intercalate (" " ++ name ++ " ") $ map (parser^.parseExpr) conds
        
        parseBetweens func colRef lower higher =
            concat
                [ parser^.parseColRef $ colRef
                , if func then "" else " NOT"
                , " BETWEEN "
                , parser^.parseColRef $ lower
                , " AND "
                , parser^.parseColRef $ higher
                ]
        parseInfix name colRef1 colRef2 =
            concat
                [ parser^.parseColRef $ colRef1
                , " "
                , name
                , " "
                , parser^.parseColRef $ colRef2
                ]
        parseIs colRef text =
            concat
                [ parser^.parseColRef $ colRef
                , "IS "
                , text
                ]
        makeExpr string colRef =
            string ++ ref
            where
                ref =
                    let cRef = parser^.parseColRef $ colRef in
                    if head cRef == '(' && last cRef == ')'
                    then cRef
                    else "(" ++ cRef ++ ")"

-- | Parse a FROM clause.
parseFromFunc :: QueryParser a -> From a -> String
parseFromFunc parser (From tableReferences) =
    "FROM " ++ intercalate ", " (map (parser^.parseTableRef) tableReferences)

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
parseHavingFunc parser (Having c) =
    "HAVING " ++ (parser^.parseExpr) (ExprWrap c)

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
            [ queryParser^.parseTableRef $ tableRef1
            , " " ++ joinType ++ " "
            , queryParser^.parseTableRef $ tableRef2
            , parseMaybe (parser^.parseJoinClause) clause
            ]
        
-- | Parse an ORDER BY clause.
parseOrderByFunc :: QueryParser a -> OrderBy a -> String
parseOrderByFunc parser clause =
    concat
        [ "ORDER BY "
        , intercalate ", " sortRefsParsed
        , parseMaybe
            (\(Limit v) -> "LIMIT " ++ show v)
            (clause^.partOrderByLimit)
        , parseMaybe
            (\(Offset v) -> "OFFSET " ++ show v)
            (clause^.partOrderByOffset)
        ]
    where
        sortRefsParsed = map (parser^.parseSortRef) (clause^.partOrderByColumns)

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
                      (parser^.parseColRef)       (sortRef^.sortRefColRefs)
        ++ parseMaybe (parser^.parseSortOrder)    (sortRef^.sortRefOrder)
        ++ parseMaybe (parser^.parseSortNull)     (sortRef^.sortRefNulls)
       
-- | Parse the name of a table.
parseTableNameFunc :: QueryParser a -> Table a -> String
parseTableNameFunc parser table = parser^.quoteElem $ table^.tableName

-- | Parse a table reference for the use in a FROM clause.
parseTableRefFunc :: StmtParser a -> QueryParser a -> TableRef a -> String
parseTableRefFunc stmtP parser join =
    case join of
        TableJoinRef    ref    alias -> concat [ maybe "" (const "(") alias
                                               , parser^.parseJoin $ ref
                                               , maybe "" (const ")") alias
                                               , maybe "" pAlias alias
                                               ]
        TableTableRef   table  alias ->    (parser^.parseTableName) table
                                        ++ fromMaybe "" (fmap pAlias alias)
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
parseTableRefAsFunc :: QueryParser a -> TableRefAs a -> String
parseTableRefAsFunc parser alias =
    "AS " ++ (parser^.quoteElem) (alias^.tableRefAliasName)

-- | Parse an input values.
parseValueFunc :: QueryParser a -> ValueWrap a -> String
parseValueFunc parser (ValueWrap val) =
    case val of
        BoolVal True     -> "TRUE"
        BoolVal False    -> "FALSE"
        DefaultVal       -> "DEFAULT"
        IntVal int       -> show int
        NullVal          -> "NULL"
        StringVal string -> parser^.quoteVal $ string
        Placeholder      -> "?"

-- | Parse a WHERE clause.  
parseWhereFunc :: QueryParser a -> Where a -> String
parseWhereFunc parser (Where condition) =
    "WHERE " ++ (parser^.parseExpr) (ExprWrap condition)

----------------------------------------
-- Join parser
----------------------------------------

--------------------
-- Functions implementation
--------------------

-- Joins parser functions.

-- | Parse the ON or USING clause of a JOIN.
parseJoinClauseFunc :: QueryParser a -> JoinClause a -> String
parseJoinClauseFunc parser jClause =
    case jClause of
        JoinClauseOn predicate ->
            "ON " ++ (parser^.parseExpr) (ExprWrap predicate)
        JoinClauseUsing cols ->
            concat
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