{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Database/Hedsql/Common/Parser/Query.hs
Description : Implementation of the SQL query parsers.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL parser. It converts the AST to a 'DOC' which can
then be rendered ('renderRaw') and executed by a SQL engine.

Alternatively, the AST can be pretty parsed using the 'renderPretty' function.
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

    -- * Utilities
    , csep
    , renderRaw
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST

import Control.Lens
import Data.List(intersperse)
import Data.Maybe
import Data.Text.Lazy(pack)
import Database.Hedsql.Common.PrettyPrint
import Prelude hiding ((<$>))

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

{-|
Apply a parsing function to a maybe.
If 'Just', returns the result of the parse function preceed by a space.
If 'Nothing', returns an empty 'Doc'.
-}
parseMaybe :: (a -> Doc) -> Maybe a -> Doc
parseMaybe f = maybe empty ((<>) space . f)

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

Defines the different computation's steps of the conversion of the AST
to a 'Doc'.
-}
data Parser a = Parser
    { _parseStmt :: Statement a -> Doc

    , _parseExpr :: ExprWrap a -> Doc

      -- | Parse a table for a CREATE TABLE statement.
    , _parseTableConst     :: TableConstraint     a -> Doc
    , _parseTableConstType :: TableConstraintType a -> Doc
    , _parseFk             :: ForeignKey          a -> Doc
    , _parseMatch          :: Match               a -> Doc
    , _parseOnAction       :: OnAction            a -> Doc
    , _parseAction         :: SqlAction           a -> Doc
    , _parseConstTiming    :: ConstraintTiming    a -> Doc

    , _parseView :: View a -> Doc

      -- | Parse a column for a CREATE TABLE statement.
    , _parseColCreate    :: Int -> ColWrap    a -> Doc
    , _parseDataType     :: DataTypeWrap      a -> Doc
    , _parseColConst     :: ColConstraint     a -> Doc
    , _parseColConstType :: ColConstraintType a -> Doc

    , _parseCreate :: Create a -> Doc
    , _parseDrop   :: Drop   a -> Doc

      -- | Parse a table for a data manipulation statement
      --   (SELECT, INSERT, UPDATE).
    , _parseTableName  :: Table      a -> Doc
    , _parseTableRef   :: TableRef   a -> Doc
    , _parseTableRefAs :: TableRefAs a -> Doc

      -- | Parse a column for a data manipulation statement
      --   (SELECT, INSERT, UPDATE).
    , _parseCol       :: ColWrap    a -> Doc
    , _parseColDef    :: ColDefWrap a -> Doc

      -- | Parse a column reference definition (selection of a SELECT query
      --   when the selection is defined.
      --   Example: ""Table1"."col1" AS "colA"
    , _parseColRefDef :: ColRefWrap a -> Doc

      -- | Parse a column reference when used in a SELECT query, using its
      --   alias if applicable.
    , _parseColRef :: ColRefWrap a -> Doc

    , _parseValue :: ValueWrap a -> Doc

    , _parseSelect      :: SelectWrap    a -> Doc
    , _parseCombination :: Combination   a -> Doc
    , _parseSelectType  :: SelectType    a -> Doc
    , _parseSelection   :: SelectionWrap a -> Doc
    , _parseFrom        :: From          a -> Doc
    , _parseJoin        :: Join          a -> Doc
    , _parseJoinClause  :: JoinClause    a -> Doc
    , _parseJoinTCol    :: JoinTypeCol   a -> Doc
    , _parseJoinTTable  :: JoinTypeTable a -> Doc
    , _parseWhere       :: Where         a -> Doc
    , _parseGroupBy     :: GroupBy       a -> Doc
    , _parseHaving      :: Having        a -> Doc
    , _parseOrderBy     :: OrderBy       a -> Doc
    , _parseSortRef     :: SortRef       a -> Doc
    , _parseSortOrder   :: SortOrder     a -> Doc
    , _parseSortNull    :: SortNulls     a -> Doc

      -- | Parse an assignment for an UPDATE statement.
    , _parseAssgnmt :: Assignment a -> Doc

      -- | Parse many assignments for an INSERT statement.
    , _parseInsertAssign :: [Assignment a] -> Doc
    , _parseDelete  :: Delete a -> Doc
    , _parseInsert  :: Insert a -> Doc
    , _parseUpdate  :: Update a -> Doc

      -- | Quote an element (table or column reference).
    , _quoteElem :: String -> Doc

      -- | Quote a value.
    , _quoteVal  :: String -> Doc
    }

----------------------------------------
-- Functions implementation
----------------------------------------

-- | Parse a CASCADE or RESTRICT action.
parseActionFunc :: SqlAction a -> Doc
parseActionFunc Cascade    = "CASCADE"
parseActionFunc NoAction   = empty
parseActionFunc Restrict   = "RESTRICT"
parseActionFunc SetDefault = "SET DEFAULT"
parseActionFunc SetNull    = "SET NULL"

-- | Parse a column which can be used for a CREATE statement.
parseColCreateFunc :: Parser a -> Int -> ColWrap a -> Doc
parseColCreateFunc parser longestName col = hsep
  [ _quoteElem parser cName
  , indent n $ _parseDataType parser (col^.colWrapType)
  , consts $ col^.colWrapConstraints
  ]
  where
    consts []   = empty
    consts cols = csep $ map (_parseColConst parser) cols

    n = longestName - length cName

    cName = col^.colWrapName

-- | Parse a column constraint type.
parseColConstTypeFunc :: Parser a -> ColConstraintType a -> Doc
parseColConstTypeFunc parser cst =
    case cst of
        Check condition ->
            "CHECK" <+> parens (_parseExpr parser $ ExprWrap condition)

        Default expr ->
            "DEFAULT" <> parens (_parseExpr parser $ ExprWrap expr)

        NotNull ->
            "NOT NULL"

        Null ->
            "NULL"

        Primary isAuto ->
            "PRIMARY KEY" <+> auto isAuto
            where
                auto True = "AUTOINCREMENT"
                auto False = empty

        Reference (ForeignKey table cols _ action) -> hsep
            [  "REFERENCES"
            ,  _parseTableName parser table
            <> parens (_quoteElem parser $ head cols ^. colWrapName)
            ,  maybe empty (_parseOnAction parser) action
            ]

        Unique ->
            "UNIQUE"

-- | Parse a column constraint.
parseColConstFunc :: Parser a -> ColConstraint a -> Doc
parseColConstFunc parser colConstraint =
        parseName                (colConstraint^.colConstraintName)
     <> _parseColConstType parser (colConstraint^.colConstraintType)
        where
            parseName (Just name) = "CONSTRAINT "
                                    <> _quoteElem parser name
                                    <> space
            parseName  Nothing    = empty

-- | Parse a timing constraint.
parseConstTimingFunc :: ConstraintTiming  a -> Doc
parseConstTimingFunc timing =
       if timing^.isConstraintDeferable then "DEFERABLE" else "NOT DEFERABLE"
    <> " INITIALLY "
    <> if timing^.isConstraintImmediate then "IMMEDIATE" else "DEFERRED"

-- | Create a CREATE statement.
parseCreateFunc :: Parser a -> Create a -> Doc
parseCreateFunc parser create =
  case create of
    CreateTable ifNotExists table ->
          "CREATE TABLE"
      <+> notExists ifNotExists
      <+> (_parseTableName parser $ table)
      <+> "("
      <$$> indent 2
             ( vsep
             $ punctuate comma
             $ map (_parseColCreate parser $ longestName table)
             $ table^.tableCols
             )
      <> (consts $ map (_parseTableConst parser) $ table^.tableConsts)
      <$$>  ")"
    CreateView ifNotExists v ->
      hsep
        [ "CREATE VIEW"
        , notExists ifNotExists
        , _parseView parser v
        ]
  where
    notExists cond = if cond then "IF NOT EXISTS" else empty

    consts [] = empty
    consts l  = ", " <> csep l

    longestName table =
      foldr (\x -> max (length $ x^.colWrapName)) 0 (table^.tableCols)

-- | Create the VIEW clause of a CREATE statement.
parseViewFunc :: Parser a -> View a -> Doc
parseViewFunc parser stmt = hsep
    [  _quoteElem parser $ stmt^.viewName
    , "AS"
    , _parseSelect parser $ stmt^.viewSelect
    ]

-- | Parse SQL data types.
parseDataTypeFunc :: DataTypeWrap a -> Doc
parseDataTypeFunc (DataTypeWrap Bool)           = "boolean"
parseDataTypeFunc (DataTypeWrap Date)           = "date"
parseDataTypeFunc (DataTypeWrap (Char lenght))  = "char" <> parens (int lenght)
parseDataTypeFunc (DataTypeWrap SmallInt)       = "smallint"
parseDataTypeFunc (DataTypeWrap Integer)        = "integer"
parseDataTypeFunc (DataTypeWrap BigInt)         = "bigint"
parseDataTypeFunc (DataTypeWrap (Varchar max')) = "varchar" <> parens (int max')
parseDataTypeFunc (DataTypeWrap Undef)          = empty

-- | Parse a FOREIGN KEY clause.
parseFkFunc :: Parser a -> ForeignKey a -> Doc
parseFkFunc parser fk = hsep
        [ _parseTableName parser $ fk^.foreignKeyTable
        , parens $ csep $ map (_parseCol parser) $ fk^.foreignKeyCols
        , makeMatch  $ fk^.foreignKeyMatch
        , makeAction $ fk^.foreignKeyAction
        ]
        where
            makeMatch (Just match) = _parseMatch parser match
            makeMatch  Nothing     = empty
            makeAction (Just action) = _parseOnAction parser action
            makeAction  Nothing      = empty

-- | Parse a MATCH clause.
parseMatchFunc :: Match a -> Doc
parseMatchFunc Full    = "FULL"
parseMatchFunc Partial = "PARTIAL"
parseMatchFunc Simple  = "SIMPLE"

-- | Parse ON DELETE or ON UPDATE clauses.
parseOnActionFunc :: Parser a -> OnAction a -> Doc
parseOnActionFunc parser (OnDelete action) =    "ON DELETE "
                                             <> _parseAction parser action
parseOnActionFunc parser (OnUpdate action) =    "ON UPDATE "
                                             <> _parseAction parser action

-- | Parse a table constraint.
parseTableConstFunc :: Parser a -> TableConstraint a -> Doc
parseTableConstFunc parser table = hsep $ catMaybes
    [ fmap  parseName                    $ table^.tableConstraintName
    , Just $ _parseTableConstType parser $ table^.tableConstraintType
    , fmap (_parseConstTiming parser)    $ table^.tableConstraintTiming
    ]
    where
        parseName name = "CONSTRAINT " <> _quoteElem parser name <> empty

-- | Parse a table constraint type.
parseTableConstTypeFunc :: Parser a -> TableConstraintType a -> Doc
parseTableConstTypeFunc parser cond =
    case cond of
        TCCheck condition -> hsep
            [ "CHECK"
            , parens $ _parseExpr parser $ ExprWrap condition
            ]

        TCForeignKey cols clause -> hsep
            [ "FOREIGN KEY"
            , parens $ parseCols cols
            , _parseFk parser clause
            ]

        TCPrimaryKey cols -> hsep
            [ "PRIMARY KEY"
            , parens $ parseCols cols
            ]

        TCUnique cols -> hsep
            [ "UNIQUE"
            , parens $ parseCols cols
            ]
    where
        parseCols cols = csep $ map (_parseCol parser) cols

-- | Parse a DELETE statement.
parseDeleteFunc :: Parser a -> Delete a -> Doc
parseDeleteFunc parser statement =
  "DELETE FROM"
  <+> (_parseTableName parser) (statement ^. deleteTable)
  <$> parseM (_parseWhere parser) (statement ^. deleteWhere)

-- | Parse a DROP TABLE statement.
parseDropFunc :: Parser a -> Drop a -> Doc
parseDropFunc parser dropClause =
  "DROP" <+>
  case dropClause of
       DropTable ifExists stmt -> hsep
         [ "TABLE"
         ,  exists ifExists
         ,  _parseTableName parser stmt
         ]
       DropView ifExists stmt -> hsep
         [ "VIEW"
         , exists ifExists
         , _parseView parser stmt
         ]
  where
    exists x = if x then "IF EXISTS" else empty

-- | Parse an INSERT statement.
parseInsertFunc :: Parser a -> Insert a -> Doc
parseInsertFunc parser insert =
      "INSERT INTO"
  <+> (_quoteElem parser) (insert ^. insertTable.tableName)
  <+> (_parseInsertAssign parser) (insert ^. insertAssign)

-- | Parse INSERT assignments.
parseInsertAssignFunc :: Parser a -> [Assignment a] -> Doc
parseInsertAssignFunc parser assigns =
      "("
  <$$> indent 2 (vsep $ punctuate comma cols)
  <> ")"
  <$> "VALUES"
  <+> "("
  <$$> indent 2 (vsep $ punctuate comma vals)
  <> ")"
  where
    cols = map (_parseCol parser . getAssignCol) assigns
    vals = map (_parseExpr parser . getAssignExpr) assigns

-- | Parse a SELECT query.
parseSelectFunc :: Parser a -> SelectWrap a -> Doc
parseSelectFunc parser (SelectWrap (Combined combination queries)) =
    vsep $ intersperse combinator $ map encapsulate queries
    where
        combinator = _parseCombination parser combination
        parseQuery = _parseSelect parser . SelectWrap
        encapsulate q@(Single _) = parseQuery q
        encapsulate q@(_)        = parens $ parseQuery q

parseSelectFunc parser (SelectWrap (Single select)) =
      "SELECT"
  <+> (_parseSelectType parser (select^.selectType))
  $+> (_parseSelection parser  (SelectionWrap $ select^.selectCols))
  <$> parseM (_parseFrom parser)    (select^.selectFrom)
  <$> parseM (_parseWhere parser)   (select^.selectWhere)
  <$> parseM (_parseGroupBy parser) (select^.selectGroupBy)
  <$> parseM (_parseHaving parser)  (select^.selectHaving)
  <$> parseM (_parseOrderBy parser) (select^.selectOrderBy)
  <$> parseM (\(Limit v)  -> "LIMIT"  <+> int v) (select^.selectLimit)
  <+> parseM (\(Offset v) -> "OFFSET" <+> int v) (select^.selectOffset)
  where

    -- Columns flow in the SELECT clause.
    infixl 4 $+> -- Infix 5 is important to get things properly aligned!
    ($+>) = if (length $ getSelectedCols $ select^.selectCols) > 1
            then (<$>)
            else (<+>)

{-|
Parse the columns of a SELECT clause.

For the pretty parsing, if there is only one column, the column stays on the
same line as the SELECT. It will lead to the following in a SELECT:
> SELECT "name"

In case of multiple columns, they come above each others with an indent of 2
spaces in the following way:
@
SELECT
  "firstName",
  "lastName",
  "age"
@
-}
parseSelectionFunc :: Parser a -> SelectionWrap a -> Doc
parseSelectionFunc parser (SelectionWrap selection)
    | nb == 0   = "*"
    | nb > 1    = indent 2 $ vsep $ mapButLast com $ map parse cols
    | otherwise = parse (head cols)
    where
        nb = length cols
        cols = getSelectedCols selection
        com x = x <> comma
        parse = (_parseColRef parser)

-- | Parse queries' combination clause such as "UNION", "EXCEPT", etc.
parseCombinationFunc :: Combination a -> Doc
parseCombinationFunc combination =
    case combination of
        Except       -> "EXCEPT"
        ExceptAll    -> "EXCEPT ALL"
        Intersect    -> "INTERSECT"
        IntersectAll -> "INTERSECT ALL"
        Union        -> "UNION"
        UnionAll     -> "UNION ALL"

parseSelectTypeFunc :: Parser a -> SelectType a -> Doc
parseSelectTypeFunc _ All                    = empty
parseSelectTypeFunc _ Distinct               = "DISTINCT"
parseSelectTypeFunc parser (DistinctOn refs) = hsep
    [ "DISTINCT ON"
    , parens $ csep $ map (_parseColRefDef parser) refs
    ]

-- | Parse a SQL statement.
parseStmtFunc :: Parser a -> Statement a -> Doc
parseStmtFunc parser stmt =
    case stmt of
        CreateStmt  s  -> _parseCreate parser s
        DeleteStmt  s  -> _parseDelete parser s
        DropStmt    s  -> _parseDrop   parser s
        InsertStmt  s  -> _parseInsert parser s
        SelectStmt  s  -> _parseSelect parser s
        UpdateStmt  s  -> _parseUpdate parser s
        Statements  xs -> vcat $ punctuate semi $ map (_parseStmt parser) xs

-- | Parse an UPDATE statement.
parseUpdateFunc :: Parser a -> Update a -> Doc
parseUpdateFunc parser update =
      "UPDATE"
  <+> _quoteElem parser (update^.updateTable.tableName)
  <$>  "SET"
  <+> csep (map (_parseAssgnmt parser) assignments)
  <$> parseM (_parseWhere parser) (update^.updateWhere)
  where
     assignments = update^.updateAssignments

{-|
Parse the assignment of an UPDATE statement.

Note: this function is located in the Query Parser because it is the only
one specific to the UPDATE statement. Thus, a dedicated UPDATE parser
for this only purpose wouldn't make a lot of sense.
-}
parseAssgnmtFunc :: Parser a -> Assignment a -> Doc
parseAssgnmtFunc parser (Assignment col val) = hsep
  [ _parseCol parser $ ColWrap col
  , "="
  , _parseExpr parser $ ExprWrap val
  ]

-- | Parse the name of a column.
parseColFunc :: Parser a -> ColWrap a -> Doc
parseColFunc parser (ColWrap col) = _quoteElem parser $ col^.colName

-- | Parse a column definition.
parseColDefFunc :: Parser a -> ColDefWrap a -> Doc
parseColDefFunc parser (ColDefWrap colDef) = hsep
    [ maybe empty ((<>) "." . _parseTableRef parser) $ colDef^.colExprTableLabel
    , _parseCol parser $ ColWrap $ colDef^.colExpr
    ]

{-|
Parse a column reference using its alias if defined.

If the column reference is a column and belongs to a specified table reference,
then a qualified name will be returned (for example: "Table1"."col1").
-}
parseColRefDefFunc :: Parser a -> ColRefWrap a -> Doc
parseColRefDefFunc parser (ColRefWrap colRef) =
        maybe ifNothing (_quoteElem parser) (colRef^.colRefLabel)
    where
        ifNothing = _parseExpr parser $ ExprWrap $ colRef^.colRefExpr

-- | Define a column reference including its alias definition if specified.
parseColRefFunc :: Parser a -> ColRefWrap a -> Doc
parseColRefFunc parser (ColRefWrap colRef) =
       _parseExpr parser (ExprWrap $ colRef^.colRefExpr)
    <+> maybe empty ((<+>) "AS" . _quoteElem parser) (colRef^.colRefLabel)

-- | Parse a SQL expression.
parseExprFunc :: Parser a -> ExprWrap a -> Doc
parseExprFunc parser (ExprWrap expr) =
    case expr of
        ColExpr (ColDef col l) -> hcat
            [ maybe
                  ""
                  (flip (<>) "." . _quoteElem parser . getTableRefName)
                  l
            , _parseCol parser $ ColWrap col
            ]
        And c1 c2 p -> pCond "AND" (ExprWrap c1) (ExprWrap c2) p
        Or  c1 c2 p -> pCond "OR"  (ExprWrap c1) (ExprWrap c2) p
        SelectExpr s -> parens $ align $ _parseSelect parser $ SelectWrap s

        Value  val -> _parseValue parser $ ValueWrap val
        Values vals ->
            parens $ csep $ map (_parseValue parser . ValueWrap) vals

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
            "EXISTS" <+> _parseColRefDef parser (ColRefWrap colRef)
        GreaterThan ref1 ref2 ->
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
        CalcFoundRows -> error $
               "SQL_CALC_FOUND_ROWS is specific to MariaDB."
            ++ "Use the MariaDB parser."
        FoundRows     -> error
              "FOUND_ROWS is specific to MariaDB. Use the MariaDB parser."

    where
        pCond name c1 c2 p =
                if p then text "(" else empty
            <>  (_parseExpr parser) c1
            <$> text name <+> (_parseExpr parser) c2
            <>  if p then text ")" else empty

        parseBetweens func colRef lower higher = hsep
                [ _parseColRefDef parser colRef
                , if func then empty else "NOT"
                , "BETWEEN"
                , _parseColRefDef parser lower
                , "AND"
                , _parseColRefDef parser higher
                ]

        parseInfix name colRef1 colRef2 = hsep
                [ _parseColRefDef parser colRef1
                , name
                , _parseColRefDef parser colRef2
                ]

        -- Parse an operator.
        parseOp name colRef1 colRef2 =
            hsep [parsePart colRef1, name, parsePart colRef2]
            where
                parsePart c@(ColRefWrap cRef) =
                    if isOperator $ ExprWrap $ cRef^.colRefExpr
                    then parens $ _parseColRefDef parser c
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

        parseIs colRef t = hsep
            [ _parseColRefDef parser colRef
            , "IS"
            , t
            ]

        makeExpr str colRef =
            str <> ref
            where
                ref =
                    let cRef = renderRaw $ _parseColRefDef parser colRef in
                    if head cRef == '(' && last cRef == ')'
                    then text $ pack $ cRef
                    else parens $ text $ pack $ cRef

-- | Parse a FROM clause.
parseFromFunc :: Parser a -> From a -> Doc
parseFromFunc parser (From [tableRef]) =
    "FROM" <+> _parseTableRef parser tableRef

parseFromFunc parser (From tableReferences) =
    "FROM" <$> indent 2 (vsep $ mapButLast (\x -> x <> comma) parsedRefs)
    where
        parsedRefs = map (_parseTableRef parser) tableReferences

{-|
Parse a GROUP BY clause.

For pretty print, if the group clause has only one column it is displayed
on one line only.
For example:
> GROUP BY "col1"

Otherwise, each column is displayed on a different line:
@
GROUP BY
  "col1",
  "col2",
  [etc.]
@
-}
parseGroupByFunc :: Parser a -> GroupBy a -> Doc
parseGroupByFunc parser clause =
    case clause of
        GroupBy [colRef] -> gb <+> parse colRef
        GroupBy colRefs  ->
                gb
            <$> indent 2 (vsep $ mapButLast (\x -> x <> comma) $ map parse colRefs)
    where
        gb = "GROUP BY"
        parse = _parseColRefDef parser

-- | Parse a HAVING clause.
-- TODO: correct this: should parse the global expression!!!
parseHavingFunc :: Parser a -> Having a -> Doc
parseHavingFunc parser havingClause =
    let
        e = getHavingExpr havingClause
        args = case e of
            ExprWrap(And _ _ _) -> ((<$>), indent 2)
            ExprWrap(Or  _ _ _) -> ((<$>), indent 2)
            _         -> ((<+>), id)
    in (fst args) "HAVING" $ snd args $ _parseExpr parser $ e

-- | Parse joins.
parseJoinFunc :: Parser a -> Join a -> Doc
parseJoinFunc parser join =
    case join of
        JoinCol joinType table1 table2 clause ->
            pJoin (_parseJoinTCol parser joinType) table1 table2 (Just clause)

        JoinTable joinType table1 table2 ->
            pJoin (_parseJoinTTable parser joinType) table1 table2 Nothing
    where
        -- Common part between column and table joins.
        pJoin joinType tableRef1 tableRef2 clause =
                _parseTableRef parser tableRef1
            <$> (joinType <+> _parseTableRef parser tableRef2)
            <$> fromMaybe empty (fmap (_parseJoinClause parser) clause)

-- | Parse an ORDER BY clause.
parseOrderByFunc :: Parser a -> OrderBy a -> Doc
parseOrderByFunc parser clause =
    getResult (clause^.orderBySortSpecList)
    where
      getResult []  = empty
      getResult [x] = "ORDER BY" <+> parse x
      getResult xs  =     "ORDER BY"
                      <$> (indent 2 $ vsep $ punctuate comma $ map parse xs)
      parse = _parseSortRef parser

-- | Parse the NULLS FIRST or NULLS LAST of a the sorting clause.
parseSortNullFunc :: SortNulls a -> Doc
parseSortNullFunc NullsFirst = "NULLS FIRST"
parseSortNullFunc NullsLast  = "NULLS LAST"

-- | Parse the ASC or DESC clauses.
parseSortOrderFunc :: SortOrder a -> Doc
parseSortOrderFunc Asc  = "ASC"
parseSortOrderFunc Desc = "DESC"

-- | Parse a sort reference.
parseSortRefFunc :: Parser a -> SortRef a -> Doc
parseSortRefFunc parser sortRef =
                      (_parseColRefDef parser)    (sortRef^.sortRefColRef)
        <> parseMaybe (_parseSortOrder parser) (sortRef^.sortRefOrder)
        <> parseMaybe (_parseSortNull parser)  (sortRef^.sortRefNulls)

-- | Parse the name of a table.
parseTableNameFunc :: Parser a -> Table a -> Doc
parseTableNameFunc parser table = _quoteElem parser $ table^.tableName

-- | Parse a table reference for the use in a FROM clause.
parseTableRefFunc :: Parser a -> TableRef a -> Doc
parseTableRefFunc parser join =
    case join of
        JoinRef ref alias -> hsep
            [  maybe empty (const "(") alias
            <> _parseJoin parser ref
            <> maybe empty (const ")") alias
            ,  maybe empty pAlias alias
            ]
        TableRef table alias ->
               _parseTableName parser table
            <+> fromMaybe empty (fmap pAlias alias)
        SelectRef select alias ->
            parens (align $ _parseSelect parser select) <+> pAlias alias
        LateralRef select alias -> hsep
            [ "LATERAL"
            , parens $ _parseSelect parser select
            , pAlias alias
            ]
    where
        pAlias alias = _parseTableRefAs parser alias

-- | Parse a table alias.
parseTableRefAsFunc :: Parser a -> TableRefAs a -> Doc
parseTableRefAsFunc parser alias =
    "AS" <+> _quoteElem parser (alias^.tableRefAsName)

-- | Parse an input values.
parseValueFunc :: Parser a -> ValueWrap a -> Doc
parseValueFunc parser (ValueWrap val) =
    case val of
        BoolVal True     -> "TRUE"
        BoolVal False    -> "FALSE"
        DefaultVal       -> "DEFAULT"
        DoubleVal      x -> double x
        FloatVal       x -> float x
        IntVal         x -> int x
        NumericVal     x -> text $ pack $ show x
        StringVal      x -> _quoteVal parser x
        GenQVal        x -> _quoteVal parser x
        GenVal         x -> text $ pack $ show x
        NullVal          -> "NULL"
        _                -> "?"

-- | Parse a WHERE clause.
parseWhereFunc :: Parser a -> Where a -> Doc
parseWhereFunc parser (Where e) =
    let
        args = case e of
            And _ _ _ -> ((<$>), indent 2)
            Or  _ _ _ -> ((<$>), indent 2)
            _         -> ((<+>), id)
    in (fst args) "WHERE" $ snd args $ _parseExpr parser $ ExprWrap e

-- | Parse the ON or USING clause of a JOIN.
parseJoinClauseFunc :: Parser a -> JoinClause a -> Doc
parseJoinClauseFunc parser jClause =
    case jClause of
        JoinClauseOn predicate ->
            "ON" <+> encapsulate predicate (makeCond predicate)
        JoinClauseUsing cols -> hsep
                [ "USING"
                , parens $ csep $ map (_parseCol parser) cols
                ]
     where
        makeCond predicate = _parseExpr parser $ ExprWrap predicate

        encapsulate :: Expression Bool a -> Doc -> Doc
        encapsulate (And _ _ _) str  = parens str
        encapsulate (Or  _ _ _) str  = parens str
        encapsulate _           str = str

-- | Parser a join on a column.
parseJoinTColFunc :: JoinTypeCol a -> Doc
parseJoinTColFunc join =
    case join of
        FullJoin  -> "FULL JOIN"
        LeftJoin  -> "LEFT JOIN"
        InnerJoin -> "INNER JOIN"
        RightJoin -> "RIGHT JOIN"

-- | Parser a join on a table.
parseJoinTTableFunc :: JoinTypeTable a -> Doc
parseJoinTTableFunc joinType =
    case joinType of
        CrossJoin        -> "CROSS JOIN"
        NaturalFullJoin  -> "NATURAL FULL JOIN"
        NaturalLeftJoin  -> "NATURAL LEFT JOIN"
        NaturalInnerJoin -> "NATURAL INNER JOIN"
        NaturalRightJoin -> "NATURAL RIGHT JOIN"

{-|
Generic quotation function.

Quotes a string such as table to "table" (the quotation parameter
will vary depending on the provided parameter).

On top of this, it will escape quote inside the string by doubling those quotes.
For example: ta"ble will become "ta""ble".

The output is a document which can then be pretty printed.
-}
genQuote ::
       Char   -- ^ Quoting character (typically ", ' or `)
    -> String -- ^ String to quote.
    -> Doc    -- ^ Returned quoted document.
genQuote quote t = text $ pack $ quotedString
  where
    quotedString  =
      concat
        [[quote]
        , concatMap (\x -> if x == quote then [quote, quote] else [x]) t
        ,[quote]
        ]

-- | Quote a SQL element, such as a table or columns names.
quoteElemFunc :: String -> Doc
quoteElemFunc = genQuote '"'

-- | Quote a raw value such as an integer.
quoteValFunc :: String -> Doc
quoteValFunc = genQuote '\''

----------------------------------------
-- Utilities
----------------------------------------

{-|
Render a 'Doc' on one single line.
-}
renderRaw :: Doc -> String
renderRaw = show . renderOneLine

{-|
Separate a list of 'Doc' with a comma and a space.
> csep ["a", "b", "c"] == "a, b, c"
-}
csep :: [Doc] -> Doc
csep = hsep . punctuate comma

-- | Map all the elements of a list but the last one.
mapButLast :: (a -> a) -> [a] -> [a]
mapButLast _ []     = []
mapButLast _ [x]    = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

parseM :: (a -> Doc) -> Maybe a -> Doc
parseM f (Just x) = f x
parseM _ _        = empty
