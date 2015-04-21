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

Implementation of the SQL parser. It converts the AST to a string which can
then be executed by a SQL engine.
-}
module Database.Hedsql.Common.Parser
    (
      -- * Default parser
      getParser
    
      -- * Parser interface
    , Parser(..)
    
      -- * Functions implementation
    , parseStmtFunc
    , parseExprFunc
    , parseTableFunc
    , parseTableConstFunc
    , parseTableConstTypeFunc
    , parseFkFunc
    , parseMatchFunc
    , parseOnActionFunc
    , parseActionFunc
    , parseConstTimingFunc
    , parseViewFunc
    , parseColCreateFunc
    , parseDataTypeFunc
    , parseColConstFunc
    , parseColConstTypeFunc
    , parseCreateFunc
    , parseDropFunc
    , parseTableNameFunc
    , parseTableRefFunc
    , parseTableRefAsFunc
    , parseColFunc
    , parseColDefFunc
    , parseColRefDefFunc
    , parseColRefFunc
    , parseValueFunc
    , parseSelectFunc
    , parseCombinationFunc
    , parseSelectTypeFunc
    , parseSelectionFunc
    , parseFromFunc
    , parseJoinFunc
    , parseJoinClauseFunc
    , parseJoinTColFunc
    , parseJoinTTableFunc
    , parseWhereFunc
    , parseGroupByFunc
    , parseHavingFunc
    , parseOrderByFunc
    , parseSortRefFunc
    , parseSortOrderFunc
    , parseSortNullFunc
    , parseAssgnmtFunc
    , parseInsertAssignFunc
    , parseDeleteFunc
    , parseInsertFunc
    , parseUpdateFunc
    , quoteElemFunc
    , quoteValFunc
    , genQuote
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST

import Control.Lens
import Data.List (intercalate)
import Data.Maybe

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

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
-- Default parser
----------------------------------------

{-|
Return the default implementation of the parser.

You can build your own parser based on this implementation through a
recursive call. For example, if you want to have a different implementation
of the _parseExpr record you could write:
> myParser = getParser myParser {_parseExpr = myFunc}
-}
getParser :: Parser a -> Parser a
getParser parser =
    Parser
      (parseStmtFunc parser)
      (parseExprFunc parser)
      (parseTableFunc parser)
      (parseTableConstFunc parser)
      (parseTableConstTypeFunc parser)
      (parseFkFunc parser)
      parseMatchFunc
      (parseOnActionFunc parser)
      parseActionFunc
      parseConstTimingFunc
      (parseViewFunc parser)
      (parseColCreateFunc parser)
      parseDataTypeFunc
      (parseColConstFunc parser)
      (parseColConstTypeFunc parser)
      (parseCreateFunc parser)
      (parseDropFunc parser)
      (parseTableNameFunc parser)
      (parseTableRefFunc parser)
      (parseTableRefAsFunc parser)
      (parseColFunc parser)
      (parseColDefFunc parser)
      (parseColRefDefFunc parser)
      (parseColRefFunc parser)
      (parseValueFunc parser)
      (parseSelectFunc parser)
      parseCombinationFunc
      (parseSelectTypeFunc parser)
      (parseSelectionFunc parser)
      (parseFromFunc parser)
      (parseJoinFunc parser)
      (parseJoinClauseFunc parser)
      parseJoinTColFunc
      parseJoinTTableFunc
      (parseWhereFunc parser)
      (parseGroupByFunc parser)
      (parseHavingFunc parser)
      (parseOrderByFunc parser)
      (parseSortRefFunc parser)
      parseSortOrderFunc
      parseSortNullFunc
      (parseAssgnmtFunc parser)
      (parseInsertAssignFunc parser)
      (parseDeleteFunc parser)
      (parseInsertFunc parser)
      (parseUpdateFunc parser)
      quoteElemFunc
      quoteValFunc

----------------------------------------
-- Interface
----------------------------------------

{-|
Interface of the parser.

Defines the different computation's steps of the AST to a string.
-}
data Parser a = Parser
    { _parseStmt :: Statement a -> String
    
    , _parseExpr :: ExprWrap a -> String
    
      -- | Parse a table for a CREATE TABLE statement.
    , _parseTable          :: Table               a -> String
    , _parseTableConst     :: TableConstraint     a -> String
    , _parseTableConstType :: TableConstraintType a -> String
    , _parseFk             :: ForeignKey          a -> String
    , _parseMatch          :: Match               a -> String
    , _parseOnAction       :: OnAction            a -> String
    , _parseAction         :: SqlAction           a -> String
    , _parseConstTiming    :: ConstraintTiming    a -> String
    
    , _parseView :: View a -> String
    
      -- | Parse a column for a CREATE TABLE statement.
    , _parseColCreate    :: ColWrap           a -> String
    , _parseDataType     :: DataTypeWrap      a -> String
    , _parseColConst     :: ColConstraint     a -> String
    , _parseColConstType :: ColConstraintType a -> String
    
    , _parseCreate :: Create a -> String
    , _parseDrop   :: Drop   a -> String
      
      -- | Parse a table for a data manipulation statement
      --   (SELECT, INSERT, UPDATE).
    , _parseTableName  :: Table      a -> String 
    , _parseTableRef   :: TableRef   a -> String
    , _parseTableRefAs :: TableRefAs a -> String
    
      -- | Parse a column for a data manipulation statement
      --   (SELECT, INSERT, UPDATE).
    , _parseCol       :: ColWrap    a -> String
    , _parseColDef    :: ColDefWrap a -> String
    
      -- | Parse a column reference definition (selection of a SELECT query
      --   when the selection is defined.
      --   Example: ""Table1"."col1" AS "colA"
    , _parseColRefDef :: ColRefWrap a -> String
    
      -- | Parse a column reference when used in a SELECT query, using its
      --   alias if applicable.
    , _parseColRef :: ColRefWrap a -> String
        
    , _parseValue :: ValueWrap a -> String
      
    , _parseSelect      :: SelectWrap    a -> String
    , _parseCombination :: Combination   a -> String
    , _parseSelectType  :: SelectType    a -> String
    , _parseSelection   :: SelectionWrap a -> String
    , _parseFrom        :: From          a -> String
    , _parseJoin        :: Join          a -> String
    , _parseJoinClause  :: JoinClause    a -> String
    , _parseJoinTCol    :: JoinTypeCol   a -> String
    , _parseJoinTTable  :: JoinTypeTable a -> String
    , _parseWhere       :: Where         a -> String
    , _parseGroupBy     :: GroupBy       a -> String
    , _parseHaving      :: Having        a -> String
    , _parseOrderBy     :: OrderBy       a -> String
    , _parseSortRef     :: SortRef       a -> String
    , _parseSortOrder   :: SortOrder     a -> String
    , _parseSortNull    :: SortNulls     a -> String
    
      -- | Parse an assignment for an UPDATE statement.
    , _parseAssgnmt :: Assignment a -> String
    
      -- | Parse many assignments for an INSERT statement.
    , _parseInsertAssign :: [Assignment a] -> String
    , _parseDelete  :: Delete a -> String
    , _parseInsert  :: Insert a -> String
    , _parseUpdate  :: Update a -> String
    
      -- | Quote an element (table or column reference).
    , _quoteElem :: String -> String
    
      -- | Quote a value.
    , _quoteVal  :: String -> String
    }

----------------------------------------
-- Functions implementation
----------------------------------------

-- | Parse a CASCADE or RESTRICT action.
parseActionFunc :: SqlAction a -> String
parseActionFunc Cascade    = "CASCADE"
parseActionFunc NoAction   = ""
parseActionFunc Restrict   = "RESTRICT"
parseActionFunc SetDefault = "SET DEFAULT"
parseActionFunc SetNull    = "SET NULL"

-- | Parse a column which can be used for a CREATE statement.
parseColCreateFunc :: Parser a -> ColWrap a -> String
parseColCreateFunc parser col = concat
    [ _quoteElem parser             $ col^.colWrapName
    , " " ++ _parseDataType parser (col^.colWrapType)
    , consts                        $ col^.colWrapConstraints
    ]
    where
        consts []   = ""
        consts cols =
            " " ++ intercalate ", " (map (_parseColConst parser) cols)

-- | Parse a column constraint type.
parseColConstTypeFunc :: Parser a -> ColConstraintType a -> String
parseColConstTypeFunc parser cst =
    case cst of
        Check condition -> 
            "CHECK (" ++ _parseExpr parser (ExprWrap condition) ++ ")"
    
        Default expr ->
            "DEFAULT(" ++ _parseExpr parser (ExprWrap expr) ++ ")"
        
        NotNull ->
            "NOT NULL"
        
        Null ->
            "NULL"
    
        Primary isAuto ->
            "PRIMARY KEY" ++ auto isAuto
                where
                    auto True = " AUTOINCREMENT"
                    auto False = ""
        
        Reference (ForeignKey table cols _ action) -> concat
            [ "REFERENCES "
            , _parseTableName parser table
            , "("
            , _quoteElem parser $ head cols^.colWrapName
            , ")"
            , maybe "" ((++) " " . _parseOnAction parser) action
            ]
         
        Unique ->
            "UNIQUE"

-- | Parse a column constraint.
parseColConstFunc :: Parser a -> ColConstraint a -> String
parseColConstFunc parser colConstraint = 
        parseName                (colConstraint^.colConstraintName)
    ++ _parseColConstType parser (colConstraint^.colConstraintType)
        where
            parseName (Just name) =    "CONSTRAINT " 
                                    ++ _quoteElem parser name
                                    ++ " "
            parseName  Nothing    = ""
        
-- | Parse a timing constraint.
parseConstTimingFunc :: ConstraintTiming  a -> String
parseConstTimingFunc timing =
       if timing^.isConstraintDeferable then "DEFERABLE" else "NOT DEFERABLE"
    ++ " INITIALLY "
    ++ if timing^.isConstraintImmediate then "IMMEDIATE" else "DEFERRED"

-- | Create a CREATE statement.
parseCreateFunc :: Parser a -> Create a -> String
parseCreateFunc parser create =
    case create of
        CreateTable ifNotExists table ->
            concat
                [ "CREATE TABLE "
                , notExists ifNotExists
                , _parseTable parser table
                ]
        CreateView ifNotExists v ->
            concat
                [ "CREATE VIEW "
                , notExists ifNotExists
                , _parseView parser v
                ]
    where
        notExists cond = if cond then "IF NOT EXISTS " else "" 

-- | Parse the TABLE clause of a CREATE statement.   
parseTableFunc :: Parser a -> Table a -> String
parseTableFunc parser stmt = 
    concat
        [ _quoteElem parser $ stmt^.tableName
        , " ("
        , intercalate ", " $ map (_parseColCreate parser) $ stmt^.tableCols
        , concat $ map ((++) ", "  . _parseTableConst parser) $ stmt^.tableConsts
        , ")"
        ]

-- | Create the VIEW clause of a CREATE statement.
parseViewFunc :: Parser a -> View a -> String 
parseViewFunc parser stmt =
    concat
        [ _quoteElem parser $ stmt^.viewName
        , " AS "
        , _parseSelect parser $ stmt^.viewSelect
        ]

-- | Parse SQL data types.
parseDataTypeFunc :: DataTypeWrap a -> String
parseDataTypeFunc (DataTypeWrap Bool)           = "boolean"
parseDataTypeFunc (DataTypeWrap Date)           = "date"
parseDataTypeFunc (DataTypeWrap (Char lenght))  = "char(" ++ show lenght ++ ")"
parseDataTypeFunc (DataTypeWrap SmallInt)       = "smallint"
parseDataTypeFunc (DataTypeWrap Integer)        = "integer"
parseDataTypeFunc (DataTypeWrap BigInt)         = "bigint"
parseDataTypeFunc (DataTypeWrap (Varchar max')) = "varchar(" ++ show max' ++ ")"
parseDataTypeFunc (DataTypeWrap Undef)          = ""

-- | Parse a FOREIGN KEY clause.
parseFkFunc :: Parser a -> ForeignKey a -> String
parseFkFunc parser fk =
    concat
        [ _parseTable parser $ fk^.foreignKeyTable
        , " ("
        , intercalate ", " $ map (_parseCol parser) $ fk^.foreignKeyCols
        , ")"
        , makeMatch  $ fk^.foreignKeyMatch
        , makeAction $ fk^.foreignKeyAction
        ]
        where
            makeMatch (Just match) = " " ++ _parseMatch parser match
            makeMatch  Nothing     = ""
            makeAction (Just action) = " " ++ _parseOnAction parser action
            makeAction  Nothing      = ""

-- | Parse a MATCH clause.
parseMatchFunc :: Match a -> String
parseMatchFunc Full    = "FULL"
parseMatchFunc Partial = "PARTIAL"
parseMatchFunc Simple  = "SIMPLE"

-- | Parse ON DELETE or ON UPDATE clauses.
parseOnActionFunc :: Parser a -> OnAction a -> String
parseOnActionFunc parser (OnDelete action) =    "ON DELETE "
                                             ++ _parseAction parser action
parseOnActionFunc parser (OnUpdate action) =    "ON UPDATE "
                                             ++ _parseAction parser action
                                             
-- | Parse a table constraint.
parseTableConstFunc :: Parser a -> TableConstraint a -> String
parseTableConstFunc parser table = concat $ catMaybes
    [ fmap  parseName                    $ table^.tableConstraintName
    , Just $ _parseTableConstType parser $ table^.tableConstraintType
    , fmap  parseTiming                  $ table^.tableConstraintTiming
    ]
    where
        parseName name = "CONSTRAINT " ++ _quoteElem parser name ++ " "
        parseTiming timing = " " ++ _parseConstTiming parser timing
        
-- | Parse a table constraint type.
parseTableConstTypeFunc :: Parser a -> TableConstraintType a -> String
parseTableConstTypeFunc parser cond =
    case cond of
        TCCheck condition -> concat
            [ "CHECK ("
            , _parseExpr parser $ ExprWrap condition
            , ")"
            ]
    
        TCForeignKey cols clause -> concat
            [ "FOREIGN KEY ("
            , parseCols cols
            , ")"
            , _parseFk parser clause
            ]
    
        TCPrimaryKey cols -> concat
            [ "PRIMARY KEY ("
            , parseCols cols
            , ")"
            ]
        
        TCUnique cols -> concat
            [ "UNIQUE ("
            , parseCols cols
            , ")"
            ]
    where
        parseCols cols = intercalate ", " $ map (_parseCol parser) cols

-- | Parse a DELETE statement.
parseDeleteFunc :: Parser a -> Delete a -> String
parseDeleteFunc parser statement =
       "DELETE FROM "
    ++            _parseTableName parser (statement^.deleteTable) 
    ++ parseMaybe (_parseWhere parser)   (statement^.deleteWhere)

-- | Parse a DROP TABLE statement.
parseDropFunc :: Parser a -> Drop a -> String
parseDropFunc parser dropClause =
    "DROP " ++
    case dropClause of
        DropTable ifExists stmt ->
                "TABLE "
            ++  exists ifExists
            ++  _parseTableName parser stmt
        DropView ifExists stmt ->
                "VIEW "
            ++  exists ifExists
            ++  _parseView parser stmt
     where
        exists x = if x then "IF EXISTS " else ""

-- | Parse an INSERT statement.
parseInsertFunc :: Parser a -> Insert a -> String
parseInsertFunc parser insert =
    concat
        [ "INSERT INTO "
        , _quoteElem parser         $ insert^.insertTable.tableName
        , " "
        , _parseInsertAssign parser $ insert^.insertAssign
        ]

-- | Parse INSERT assignments.
parseInsertAssignFunc :: Parser a -> [Assignment a] -> String        
parseInsertAssignFunc parser assigns =
    concat
        [ "("
        , intercalate ", " $ parseCols assigns
        , ") VALUES ("
        , intercalate ", " $ map (_parseExpr parser . getAssignExpr) assigns
        , ")"
        ]
     where
        parseCols = map (_parseCol parser . getAssignCol)  

-- | Parse a SELECT query.
parseSelectFunc :: Parser a -> SelectWrap a -> String
parseSelectFunc parser (SelectWrap (Combined combination queries)) =
    intercalate combinator $ map encapsulate queries
    where
        combinator = " " ++ _parseCombination parser combination ++ " "
        parseQuery = _parseSelect parser . SelectWrap
        encapsulate q@(Single _) = parseQuery q        
        encapsulate q@(_)        = "(" ++ parseQuery q ++ ")"
parseSelectFunc parser (SelectWrap (Single select)) =
      concat $ catMaybes
        [ Just   "SELECT "
        , Just $ _parseSelectType parser $ select^.selectType
        , Just $ _parseSelection parser  $ SelectionWrap $ select^.selectCols
        , parseM (_parseFrom parser)     $ select^.selectFrom
        , parseM (_parseWhere parser)    $ select^.selectWhere
        , parseM (_parseGroupBy parser)  $ select^.selectGroupBy
        , parseM (_parseOrderBy parser)  $ select^.selectOrderBy
        ]
    where
        parseM f = fmap $ (++) " " . f

parseSelectionFunc :: Parser a -> SelectionWrap a -> String
parseSelectionFunc parser (SelectionWrap selection) =
    intercalate ", " $ map (_parseColRef parser) (getSelectedCols selection)

-- | Parse queries' combination clause such as "UNION", "EXCEPT", etc.
parseCombinationFunc :: Combination a -> String
parseCombinationFunc combination =
    case combination of
        Except       -> "EXCEPT"
        ExceptAll    -> "EXCEPT ALL"
        Intersect    -> "INTERSECT"
        IntersectAll -> "INTERSECT ALL"
        Union        -> "UNION"
        UnionAll     -> "UNION ALL"

parseSelectTypeFunc :: Parser a -> SelectType a -> String
parseSelectTypeFunc _ All                    = ""
parseSelectTypeFunc _ Distinct               = "DISTINCT "
parseSelectTypeFunc parser (DistinctOn refs) =
    concat
        [ "DISTINCT ON ("
        , intercalate ", " $ map (_parseColRefDef parser) refs
        , ") "
        ]

-- | Parse a SQL statement.
parseStmtFunc :: Parser a -> Statement a -> String
parseStmtFunc parser stmt =
    case stmt of
        CreateStmt  s  -> _parseCreate parser s
        DeleteStmt  s  -> _parseDelete parser s
        DropStmt    s  -> _parseDrop   parser s
        InsertStmt  s  -> _parseInsert parser s
        SelectStmt  s  -> _parseSelect parser s
        UpdateStmt  s  -> _parseUpdate parser s
        Statements  xs -> xs >>= flip (++) "; " . _parseStmt parser                               

-- | Parse an UPDATE statement.
parseUpdateFunc :: Parser a -> Update a -> String    
parseUpdateFunc parser update = 
    concat $ catMaybes
        [ Just   "UPDATE "
        , Just $ _quoteElem parser $ update^.updateTable.tableName
        , Just   " SET "
        , Just $ intercalate ", " $ map (_parseAssgnmt parser) assignments
        , fmap ((++) " " . _parseWhere parser) $ update^.updateWhere
        ]
    where
        assignments = update^.updateAssignments

{-|
Parse the assignment of an UPDATE statement.

Note: this function is located in the Query Parser because it is the only
one specific to the UPDATE statement. Thus, a dedicated UPDATE parser
for this only purpose wouldn't make a lot of sense.
-}
parseAssgnmtFunc :: Parser a -> Assignment a -> String
parseAssgnmtFunc parser (Assignment col val) =
    concat
        [ _parseCol parser $ ColWrap col
        , " = "
        , _parseExpr parser $ ExprWrap val
        ]

-- | Parse the name of a column.
parseColFunc :: Parser a -> ColWrap a -> String
parseColFunc parser (ColWrap col) = _quoteElem parser $ col^.colName

-- | Parse a column definition.
parseColDefFunc :: Parser a -> ColDefWrap a -> String
parseColDefFunc parser (ColDefWrap colDef) = concat
    [ maybe "" ((++) "." . _parseTableRef parser) $ colDef^.colExprTableLabel
    , _parseCol parser $ ColWrap $ colDef^.colExpr
    ]
        
{-|
Parse a column reference using its alias if defined.

If the column reference is a column and belongs to a specified table reference,
then a qualified name will be returned (for example: "Table1"."col1").
-}
parseColRefDefFunc :: Parser a -> ColRefWrap a -> String
parseColRefDefFunc parser (ColRefWrap colRef) =
        maybe ifNothing (_quoteElem parser) (colRef^.colRefLabel)
    where
        ifNothing = _parseExpr parser $ ExprWrap $ colRef^.colRefExpr
        
-- | Define a column reference including its alias definition if specified.
parseColRefFunc :: Parser a -> ColRefWrap a -> String
parseColRefFunc parser (ColRefWrap colRef) =
       _parseExpr parser (ExprWrap $ colRef^.colRefExpr)
    ++ maybe "" ((++) " AS " . _quoteElem parser) (colRef^.colRefLabel)

-- | Parse a SQL expression.
parseExprFunc :: Parser a -> ExprWrap a -> String
parseExprFunc parser (ExprWrap expr) =
    case expr of
        ColExpr (ColDef col l) ->
            concat
                [ maybe
                      ""
                      (flip (++) "." . _quoteElem parser . getTableRefName)
                      l
                , _parseCol parser $ ColWrap col
                ]
        And conds -> pCond "AND" $ map ExprWrap conds
        Or  conds -> pCond "OR"  $ map ExprWrap conds    
        SelectExpr select ->
            concat ["(", _parseSelect parser $ SelectWrap select, ")"]
            
        Value  val ->
            _parseValue parser $ ValueWrap val
        Values vals ->
            concat
                ["("
                , intercalate ", " $ map (_parseValue parser . ValueWrap) vals
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
            "EXISTS " ++ _parseColRefDef parser (ColRefWrap colRef)
        GreaterThan       ref1 ref2 ->
            parseInfix ">" (ColRefWrap ref1) (ColRefWrap ref2)
        GreaterThanOrEqTo ref1 ref2 ->
            parseInfix ">=" (ColRefWrap ref1) (ColRefWrap ref2)
        In                ref1 ref2 ->
            parseInfix "IN" (ColRefWrap ref1) (ColRefWrap ref2)
        IsDistinctFrom    ref1 ref2 ->
            parseInfix "IS DISTINCT FROM" (ColRefWrap ref1) (ColRefWrap ref2)
        IsFalse e -> parseIs (ColRefWrap e) "FALSE"
        IsNotDistinctFrom ref1 ref2 -> parseInfix
                                            "IS NOT DISTINCT FROM"
                                            (ColRefWrap ref1)
                                            (ColRefWrap ref2)
        IsNotFalse        e      -> parseIs (ColRefWrap e) "NOT FALSE"
        IsNotNull         e      -> parseIs (ColRefWrap e) "NOT NULL"
        IsNotTrue         e      -> parseIs (ColRefWrap e) "NOT TRUE"
        IsNotUnknown      e      -> parseIs (ColRefWrap e) "NOT UNKNOWN"
        IsNull            e      -> parseIs (ColRefWrap e) "NULL"
        IsTrue            e      -> parseIs (ColRefWrap e) "TRUE"
        IsUnknown         e      -> parseIs (ColRefWrap e) "UNKNOWN"
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
            parseOp "+" (ColRefWrap left) (ColRefWrap right)
        BitAnd left right -> parseOp "&" (ColRefWrap left) (ColRefWrap right)
        BitOr left right -> parseOp "|" (ColRefWrap left) (ColRefWrap right)
        BitShiftLeft  left right ->
            parseOp "<<" (ColRefWrap left) (ColRefWrap right)
        BitShiftRight left right ->
            parseOp ">>" (ColRefWrap left) (ColRefWrap right)
        Divide left right ->
            parseOp "/" (ColRefWrap left) (ColRefWrap right)
        Modulo left right ->
            parseOp "%" (ColRefWrap left) (ColRefWrap right)
        Multiply left right ->
            parseOp "*" (ColRefWrap left) (ColRefWrap right)
        Substract left right ->
            parseOp "-" (ColRefWrap left) (ColRefWrap right) 
    
        -- Functions.
        Count       e -> makeExpr "COUNT" (ColRefWrap e)
        CurrentDate   -> "CURRENT_DATE"
        Joker         -> "*"
        Max         e -> makeExpr "MAX" (ColRefWrap e)
        Min         e -> makeExpr "MIN" (ColRefWrap e)
        Random        -> "random()"
        Sum         e -> makeExpr "SUM" (ColRefWrap e)
        
        -- MariaDB functions.
        CalcFoundRows -> error
               "SQL_CALC_FOUND_ROWS is specific to MariaDB."
            ++ "Use the MariaDB parser."
        FoundRows     -> error
              "FOUND_ROWS is specific to MariaDB. Use the MariaDB parser."
    
    where
        pCond name conds =
            intercalate (" " ++ name ++ " ") $ map (_parseExpr parser) conds
        
        parseBetweens func colRef lower higher =
            concat
                [ _parseColRefDef parser colRef
                , if func then "" else " NOT"
                , " BETWEEN "
                , _parseColRefDef parser lower
                , " AND "
                , _parseColRefDef parser higher
                ]
        
        parseInfix name colRef1 colRef2 =
            concat
                [ _parseColRefDef parser colRef1
                , " "
                , name
                , " "
                , _parseColRefDef parser colRef2
                ]
        
        -- Parse an operator.
        parseOp name colRef1 colRef2 =
            concat [parsePart colRef1, " ", name, " ", parsePart colRef2]
            where
                parsePart c@(ColRefWrap cRef) =
                    if isOperator $ ExprWrap $ cRef^.colRefExpr
                    then "(" ++ _parseColRefDef parser c ++ ")"
                    else  _parseColRefDef parser c
        
        -- Return True if an expression is an operator.
        isOperator :: ExprWrap a -> Bool
        isOperator (ExprWrap e) =
            case e of
                Add _ _           -> True
                BitAnd _ _        -> True
                BitOr _ _         -> True
                BitShiftLeft  _ _ -> True
                BitShiftRight _ _ -> True
                Divide _ _        -> True
                Modulo _ _        -> True
                Multiply _ _      -> True
                Substract _ _     -> True
                _                 -> False
        
        parseIs colRef text =
            concat
                [ _parseColRefDef parser colRef
                , "IS "
                , text
                ]
        
        makeExpr string colRef =
            string ++ ref
            where
                ref =
                    let cRef = _parseColRefDef parser colRef in
                    if head cRef == '(' && last cRef == ')'
                    then cRef
                    else "(" ++ cRef ++ ")"

-- | Parse a FROM clause.
parseFromFunc :: Parser a -> From a -> String
parseFromFunc parser (From tableReferences) =
    "FROM " ++ intercalate ", " (map (_parseTableRef parser) tableReferences)

-- | Parse a GROUP BY clause.
parseGroupByFunc :: Parser a -> GroupBy a -> String
parseGroupByFunc parser (GroupBy colRefs having) =
    concat
        [ "GROUP BY "
        , intercalate ", " $ map (_parseColRefDef parser) colRefs
        , parseMaybe (_parseHaving parser) having
        ] 

-- | Parse a HAVING clause.
parseHavingFunc :: Parser a -> Having a -> String
parseHavingFunc parser havingClause =
    "HAVING " ++ _parseExpr parser (getHavingExpr havingClause)

-- | Parse joins.
parseJoinFunc :: Parser a -> Join a -> String
parseJoinFunc parser join =
    case join of
        JoinCol joinType table1 table2 clause ->
            pJoin (_parseJoinTCol parser joinType) table1 table2 (Just clause)
        
        JoinTable joinType table1 table2 ->
            pJoin (_parseJoinTTable parser joinType) table1 table2 Nothing
    where 
        -- Common part between column and table joins.
        pJoin joinType tableRef1 tableRef2 clause = concat
            [ _parseTableRef parser tableRef1
            , " " ++ joinType ++ " "
            , _parseTableRef parser tableRef2
            , parseMaybe (_parseJoinClause parser) clause
            ]
        
-- | Parse an ORDER BY clause.
parseOrderByFunc :: Parser a -> OrderBy a -> String
parseOrderByFunc parser clause =
    concat
        [ "ORDER BY "
        , intercalate ", " sortRefsParsed
        , parseMaybe
            (\(Limit v) -> "LIMIT " ++ show v)
            (clause^.orderByLimit)
        , parseMaybe
            (\(Offset v) -> "OFFSET " ++ show v)
            (clause^.orderByOffset)
        ]
    where
        sortRefsParsed = map (_parseSortRef parser) (clause^.orderByCols)

-- | Parse the NULLS FIRST or NULLS LAST of a the sorting clause.
parseSortNullFunc :: SortNulls a -> String
parseSortNullFunc NullsFirst = "NULLS FIRST"
parseSortNullFunc NullsLast  = "NULLS LAST"

-- | Parse the ASC or DESC clauses.
parseSortOrderFunc :: SortOrder a -> String
parseSortOrderFunc Asc  = "ASC"
parseSortOrderFunc Desc = "DESC"

-- | Parse a sort reference.
parseSortRefFunc :: Parser a -> SortRef a -> String
parseSortRefFunc parser sortRef =
                      (_parseColRefDef parser)    (sortRef^.sortRefColRef)
        ++ parseMaybe (_parseSortOrder parser) (sortRef^.sortRefOrder)
        ++ parseMaybe (_parseSortNull parser)  (sortRef^.sortRefNulls)
       
-- | Parse the name of a table.
parseTableNameFunc :: Parser a -> Table a -> String
parseTableNameFunc parser table = _quoteElem parser $ table^.tableName

-- | Parse a table reference for the use in a FROM clause.
parseTableRefFunc :: Parser a -> TableRef a -> String
parseTableRefFunc parser join =
    case join of
        JoinRef ref alias ->
            concat
                [ maybe "" (const "(") alias
                , _parseJoin parser ref
                , maybe "" (const ")") alias
                , maybe "" pAlias alias
                ]
        TableRef table alias ->
               _parseTableName parser table
            ++ fromMaybe "" (fmap pAlias alias)
        SelectRef select alias ->
            concat
                [ "("
                , _parseSelect parser select
                , ")"
                , pAlias alias
                ]
        LateralRef select alias ->
            concat
                [ "LATERAL ("
                , _parseSelect parser select
                , ")"
                , pAlias alias
                ]
    where
        pAlias alias = " " ++ _parseTableRefAs parser alias

-- | Parse a table alias.
parseTableRefAsFunc :: Parser a -> TableRefAs a -> String
parseTableRefAsFunc parser alias =
    "AS " ++ _quoteElem parser (alias^.tableRefAsName)

-- | Parse an input values.
parseValueFunc :: Parser a -> ValueWrap a -> String
parseValueFunc parser (ValueWrap val) =
    case val of
        BoolVal True     -> "TRUE"
        BoolVal False    -> "FALSE"
        DefaultVal       -> "DEFAULT"
        DoubleVal      x -> show x
        FloatVal       x -> show x
        IntVal         x -> show x
        NumericVal     x -> show x
        StringVal      x -> _quoteVal parser x
        UndefStringVal x -> _quoteVal parser x
        UndefBoolVal   x -> _parseValue parser $ ValueWrap $ BoolVal x
        UndefNumVal    x -> show x
        NullVal          -> "NULL"
        NullBool         -> "NULL"
        NullNum          -> "NULL"
        NullFloat        -> "NULL"
        NullDouble       -> "NULL"
        NullInt          -> "NULL"
        NullString       -> "NULL"
        _                -> "?"

-- | Parse a WHERE clause.  
parseWhereFunc :: Parser a -> Where a -> String
parseWhereFunc parser (Where condition) =
    "WHERE " ++ _parseExpr parser (ExprWrap condition)

-- | Parse the ON or USING clause of a JOIN.
parseJoinClauseFunc :: Parser a -> JoinClause a -> String
parseJoinClauseFunc parser jClause =
    case jClause of
        JoinClauseOn predicate ->
            "ON " ++ encapsulate predicate (makeCond predicate)
        JoinClauseUsing cols ->
            concat
                [ "USING ("
                , intercalate ", " $ map (_parseCol parser) cols
                , ")"
                ]
     where
        makeCond predicate = _parseExpr parser $ ExprWrap predicate
        
        encapsulate :: Expression Bool a -> String -> String
        encapsulate (And _) string = "(" ++ string ++ ")"
        encapsulate (Or  _) string = "(" ++ string ++ ")"
        encapsulate _       string = string

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
        
{-|
Generic quotation function.

Quotes a text such as table to "table" (the quotation parameter
will vary depending on the provided parameter).

On top of this, it will escape quote inside the string by doubling those quotes.
For example: ta"ble will become "ta""ble".
-}
genQuote ::
       Char   -- ^ Quoting character (typically ", ' or `)
    -> String -- ^ String to quote.
    -> String -- ^ Returned quoted string.
genQuote quote text =
    concat
        [[quote]
        , concatMap (\x -> if x == quote then [quote, quote] else [x]) text
        ,[quote]
        ]

-- | Quote a SQL element, such as a table or columns names.
quoteElemFunc :: String -> String
quoteElemFunc = genQuote '"'

-- | Quote a raw value such as an integer.
quoteValFunc :: String -> String
quoteValFunc = genQuote '\''