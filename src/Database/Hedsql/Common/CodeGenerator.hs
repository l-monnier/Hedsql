{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Database/Hedsql/Common/CodeGenerator.hs
Description : Generic SQL code generator.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL code generator.
It converts the AST to a 'DOC' which can then be rendered ('renderRaw') and
executed by a SQL engine.

Alternatively, the AST can be pretty rendered using 'show'.
-}
module Database.Hedsql.Common.CodeGenerator
    (
      -- * Default code generator
      getCodeGenerator

      -- * Code generator interface
    , CodeGenerator(..)

      -- * Functions implementation
    , codeGenStmtFunc
    , codeGenExprFunc
    , codeGenTableConstFunc
    , codeGenTableConstTypeFunc
    , codeGenFkFunc
    , codeGenMatchFunc
    , codeGenOnActionFunc
    , codeGenActionFunc
    , codeGenConstTimingFunc
    , codeGenViewFunc
    , codeGenColCreateFunc
    , codeGenDataTypeFunc
    , codeGenColConstFunc
    , codeGenColConstTypeFunc
    , codeGenCreateFunc
    , codeGenDropFunc
    , codeGenTableNameFunc
    , codeGenTableRefFunc
    , codeGenTableRefAsFunc
    , codeGenColFunc
    , codeGenColDefFunc
    , codeGenColRefDefFunc
    , codeGenColRefFunc
    , codeGenValueFunc
    , codeGenSelectFunc
    , codeGenCombinationFunc
    , codeGenSelectTypeFunc
    , codeGenSelectionFunc
    , codeGenFromFunc
    , codeGenJoinFunc
    , codeGenJoinClauseFunc
    , codeGenJoinTColFunc
    , codeGenJoinTTableFunc
    , codeGenWhereFunc
    , codeGenGroupByFunc
    , codeGenHavingFunc
    , codeGenOrderByFunc
    , codeGenSortRefFunc
    , codeGenSortOrderFunc
    , codeGenSortNullFunc
    , codeGenAssgnmtFunc
    , codeGenInsertAssignFunc
    , codeGenDeleteFunc
    , codeGenInsertFunc
    , codeGenUpdateFunc
    , codeGenReturningFunc
    , quoteElemFunc
    , quoteValFunc
    , genQuote

    -- * Utilities
    , csep
    , renderRaw
    , renderP
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.Common.AST

import           Control.Lens
import           Data.List                          (intersperse)
import           Data.Maybe
import qualified Data.Text.Lazy as T
import           Database.Hedsql.Common.PrettyPrint
import           Prelude                            hiding ((<$>))

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Map all the elements of a list but the last one.
mapButLast :: (a -> a) -> [a] -> [a]
mapButLast _ []     = []
mapButLast _ [x]    = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

{-|
Apply  a code generation function to a maybe value if that value is 'Just'.
If the value is 'Nothing' return the empty document.
-}
codeGenM :: (a -> Doc) -> Maybe a -> Doc
codeGenM f (Just x) = f x
codeGenM _ _        = empty

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- Default code generator
----------------------------------------

{-|
Return the default implementation of the code generator.

You can build your own code generator based on this implementation through a
recursive call. For example, if you want to have a different implementation
of the _codeGenExpr record you could write:
> myCodeGenerator = getCodeGenerator myCodeGenerator {_codeGenExpr = myFunc}
-}
getCodeGenerator :: CodeGenerator a -> CodeGenerator a
getCodeGenerator codeGenerator =
  CodeGenerator
    (codeGenStmtFunc codeGenerator)
    (codeGenExprFunc codeGenerator)
    (codeGenTableConstFunc codeGenerator)
    (codeGenTableConstTypeFunc codeGenerator)
    (codeGenFkFunc codeGenerator)
    codeGenMatchFunc
    (codeGenOnActionFunc codeGenerator)
    codeGenActionFunc
    codeGenConstTimingFunc
    (codeGenViewFunc codeGenerator)
    (codeGenColCreateFunc codeGenerator)
    codeGenDataTypeFunc
    (codeGenColConstFunc codeGenerator)
    (codeGenColConstTypeFunc codeGenerator)
    (codeGenCreateFunc codeGenerator)
    (codeGenDropFunc codeGenerator)
    (codeGenTableNameFunc codeGenerator)
    (codeGenTableRefFunc codeGenerator)
    (codeGenTableRefAsFunc codeGenerator)
    (codeGenColFunc codeGenerator)
    (codeGenColDefFunc codeGenerator)
    (codeGenColRefDefFunc codeGenerator)
    (codeGenColRefFunc codeGenerator)
    (codeGenValueFunc codeGenerator)
    (codeGenSelectFunc codeGenerator)
    codeGenCombinationFunc
    (codeGenSelectTypeFunc codeGenerator)
    (codeGenSelectionFunc codeGenerator)
    (codeGenFromFunc codeGenerator)
    (codeGenJoinFunc codeGenerator)
    (codeGenJoinClauseFunc codeGenerator)
    codeGenJoinTColFunc
    codeGenJoinTTableFunc
    (codeGenWhereFunc codeGenerator)
    (codeGenGroupByFunc codeGenerator)
    (codeGenHavingFunc codeGenerator)
    (codeGenOrderByFunc codeGenerator)
    (codeGenSortRefFunc codeGenerator)
    codeGenSortOrderFunc
    codeGenSortNullFunc
    (codeGenAssgnmtFunc codeGenerator)
    (codeGenInsertAssignFunc codeGenerator)
    (codeGenDeleteFunc codeGenerator)
    (codeGenInsertFunc codeGenerator)
    (codeGenUpdateFunc codeGenerator)
    (codeGenReturningFunc codeGenerator)
    quoteElemFunc
    quoteValFunc

----------------------------------------
-- Interface
----------------------------------------

{-|
Interface of the code generator.

Defines the different computation's steps of the conversion of the AST
to a 'Doc'.
-}
data CodeGenerator dbVendor = CodeGenerator
    { _codeGenStmt           :: Statement dbVendor -> Doc

    , _codeGenExpr           :: ExprWrap dbVendor -> Doc

      -- | Generate code for a table in a CREATE TABLE statement.
    , _codeGenTableConst     :: TableConstraint     dbVendor -> Doc
    , _codeGenTableConstType :: TableConstraintType dbVendor -> Doc
    , _codeGenFk             :: ForeignKey          dbVendor -> Doc
    , _codeGenMatch          :: Match               dbVendor -> Doc
    , _codeGenOnAction       :: OnAction            dbVendor -> Doc
    , _codeGenAction         :: SqlAction           dbVendor -> Doc
    , _codeGenConstTiming    :: ConstraintTiming    dbVendor -> Doc

    , _codeGenView           :: View dbVendor -> Doc

      -- | Generate code for a column in a CREATE TABLE statement.
    , _codeGenColCreate      :: Int -> ColWrap    dbVendor -> Doc
    , _codeGenDataType       :: DataTypeWrap      dbVendor -> Doc
    , _codeGenColConst       :: ColConstraint     dbVendor -> Doc
    , _codeGenColConstType   :: ColConstraintType dbVendor -> Doc

    , _codeGenCreate         :: Create dbVendor -> Doc
    , _codeGenDrop           :: Drop   dbVendor -> Doc

      -- | Generate code for a table in a data manipulation statement
      --   (SELECT, INSERT, UPDATE).
    , _codeGenTableName      :: Table      dbVendor -> Doc
    , _codeGenTableRef       :: TableRef   dbVendor -> Doc
    , _codeGenTableRefAs     :: TableRefAs dbVendor -> Doc

      -- | Generate code for a column in a data manipulation statement
      --   (SELECT, INSERT, UPDATE).
    , _codeGenCol            :: ColWrap    dbVendor -> Doc
    , _codeGenColDef         :: ColDefWrap dbVendor -> Doc

      -- | Generate code for a column reference definition
      -- (selection of a SELECT query when the selection is defined.
      --   Example: ""Table1"."col1" AS "colA"
    , _codeGenColRefDef      :: ColRefWrap dbVendor -> Doc

      -- | Generate code for a column reference when used in a SELECT query,
      -- using its alias if applicable.
    , _codeGenColRef         :: ColRefWrap dbVendor -> Doc

    , _codeGenValue          :: ValueWrap dbVendor -> Doc

    , _codeGenSelect         :: SelectWrap    dbVendor -> Doc
    , _codeGenCombination    :: Combination   dbVendor -> Doc
    , _codeGenSelectType     :: SelectType    dbVendor -> Doc
    , _codeGenSelection      :: SelectionWrap dbVendor -> Doc
    , _codeGenFrom           :: From          dbVendor -> Doc
    , _codeGenJoin           :: Join          dbVendor -> Doc
    , _codeGenJoinClause     :: JoinClause    dbVendor -> Doc
    , _codeGenJoinTCol       :: JoinTypeCol   dbVendor -> Doc
    , _codeGenJoinTTable     :: JoinTypeTable dbVendor -> Doc
    , _codeGenWhere          :: Where         dbVendor -> Doc
    , _codeGenGroupBy        :: GroupBy       dbVendor -> Doc
    , _codeGenHaving         :: Having        dbVendor -> Doc
    , _codeGenOrderBy        :: OrderBy       dbVendor -> Doc
    , _codeGenSortRef        :: SortRef       dbVendor -> Doc
    , _codeGenSortOrder      :: SortOrder     dbVendor -> Doc
    , _codeGenSortNull       :: SortNulls     dbVendor -> Doc

      -- | Generate code for an assignment in an UPDATE statement.
    , _codeGenAssgnmt        :: Assignment dbVendor -> Doc

      -- | Generate code for many assignments in an INSERT statement.
    , _codeGenInsertAssign   :: [Assignment dbVendor] -> Doc
    , _codeGenDelete         :: DeleteWrap dbVendor -> Doc
    , _codeGenInsert         :: InsertWrap dbVendor -> Doc
    , _codeGenUpdate         :: UpdateWrap dbVendor -> Doc

    , _codeGenReturning      :: ReturningWrap dbVendor -> Doc

      -- | Quote an element (table or column reference).
    , _quoteElem           :: String -> Doc

      -- | Quote a value.
    , _quoteVal            :: String -> Doc
    }

----------------------------------------
-- Functions implementation
----------------------------------------

-- | Generate code for a CASCADE or RESTRICT action.
codeGenActionFunc :: SqlAction dbVendor -> Doc
codeGenActionFunc Cascade    = "CASCADE"
codeGenActionFunc NoAction   = empty
codeGenActionFunc Restrict   = "RESTRICT"
codeGenActionFunc SetDefault = "SET DEFAULT"
codeGenActionFunc SetNull    = "SET NULL"

-- | Generate code for a column which can be used for a CREATE statement.
codeGenColCreateFunc :: CodeGenerator dbVendor -> Int -> ColWrap dbVendor -> Doc
codeGenColCreateFunc codeGenerator longestName col = hsep
    [ _quoteElem codeGenerator cName
    , indent n $ _codeGenDataType codeGenerator (col^.colWrapType)
    , consts $ col^.colWrapConstraints
    ]
    where
        consts []   = empty
        consts cols = csep $ map (_codeGenColConst codeGenerator) cols

        n = longestName - length cName

        cName = col^.colWrapName

-- | Generate code for a column constraint type.
codeGenColConstTypeFunc :: CodeGenerator dbVendor -> ColConstraintType dbVendor -> Doc
codeGenColConstTypeFunc codeGenerator cst =
    case cst of
        Check condition ->
            "CHECK" <+> parens (_codeGenExpr codeGenerator $ ExprWrap condition)

        Default expr ->
            "DEFAULT" <> parens (_codeGenExpr codeGenerator $ ExprWrap expr)

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
            ,  _codeGenTableName codeGenerator table
            <> parens (_quoteElem codeGenerator $ head cols ^. colWrapName)
            ,  maybe empty (_codeGenOnAction codeGenerator) action
            ]

        Unique ->
            "UNIQUE"

-- | Generate code for a column constraint.
codeGenColConstFunc :: CodeGenerator dbVendor -> ColConstraint dbVendor -> Doc
codeGenColConstFunc codeGenerator colConstraint =
       codeGenName                 (colConstraint^.colConstraintName)
    <> _codeGenColConstType codeGenerator (colConstraint^.colConstraintType)
    where
        codeGenName (Just name) = "CONSTRAINT "
                                <> _quoteElem codeGenerator name
                                <> space
        codeGenName  Nothing    = empty

-- | Generate code for a timing constraint.
codeGenConstTimingFunc :: ConstraintTiming dbVendor -> Doc
codeGenConstTimingFunc timing =
       if timing^.isConstraintDeferable then "DEFERABLE" else "NOT DEFERABLE"
    <> " INITIALLY "
    <> if timing^.isConstraintImmediate then "IMMEDIATE" else "DEFERRED"

-- | Create a CREATE statement.
codeGenCreateFunc :: CodeGenerator dbVendor -> Create dbVendor -> Doc
codeGenCreateFunc codeGenerator create =
    case create of
        CreateTable ifNotExists table ->
                 "CREATE TABLE"
            <+>  notExists ifNotExists
            <+>  (_codeGenTableName codeGenerator $ table)
            <+>  "("
            <$$> indent 2
                     ( vsep
                     $ punctuate comma
                     $ map (_codeGenColCreate codeGenerator $ longestName table)
                     $ table^.tableCols
                     )
            <>   (consts $ map (_codeGenTableConst codeGenerator) $ table^.tableConsts)
            <$$> ")"
        CreateView ifNotExists v ->
            hsep
                [ "CREATE VIEW"
                , notExists ifNotExists
                , _codeGenView codeGenerator v
                ]
    where
      notExists cond = if cond then "IF NOT EXISTS" else empty

      consts [] = empty
      consts l  = ", " <> csep l

      longestName table =
          foldr (\x -> max (length $ x^.colWrapName)) 0 (table^.tableCols)

-- | Create the VIEW clause of a CREATE statement.
codeGenViewFunc :: CodeGenerator dbVendor -> View dbVendor -> Doc
codeGenViewFunc codeGenerator stmt = hsep
    [  _quoteElem codeGenerator $ stmt^.viewName
    , "AS"
    , _codeGenSelect codeGenerator $ stmt^.viewSelect
    ]

-- | Generate code for SQL data types.
codeGenDataTypeFunc :: DataTypeWrap dbVendor -> Doc
codeGenDataTypeFunc (DataTypeWrap Bool)           = "boolean"
codeGenDataTypeFunc (DataTypeWrap Date)           = "date"
codeGenDataTypeFunc (DataTypeWrap (Char lenght))  = "char" <> parens (int lenght)
codeGenDataTypeFunc (DataTypeWrap SmallInt)       = "smallint"
codeGenDataTypeFunc (DataTypeWrap Integer)        = "integer"
codeGenDataTypeFunc (DataTypeWrap BigInt)         = "bigint"
codeGenDataTypeFunc (DataTypeWrap (Varchar max')) = "varchar" <> parens (int max')
codeGenDataTypeFunc (DataTypeWrap Undef)          = empty

-- | Generate code for a FOREIGN KEY clause.
codeGenFkFunc :: CodeGenerator dbVendor -> ForeignKey dbVendor -> Doc
codeGenFkFunc codeGenerator fk = hsep
    [ _codeGenTableName codeGenerator $ fk^.foreignKeyTable
    , parens $ csep $ map (_codeGenCol codeGenerator) $ fk^.foreignKeyCols
    , makeMatch  $ fk^.foreignKeyMatch
    , makeAction $ fk^.foreignKeyAction
    ]
    where
        makeMatch (Just match) = _codeGenMatch codeGenerator match
        makeMatch  Nothing     = empty
        makeAction (Just action) = _codeGenOnAction codeGenerator action
        makeAction  Nothing      = empty

-- | Generate code for a MATCH clause.
codeGenMatchFunc :: Match dbVendor -> Doc
codeGenMatchFunc Full    = "FULL"
codeGenMatchFunc Partial = "PARTIAL"
codeGenMatchFunc Simple  = "SIMPLE"

-- | Generate code for ON DELETE or ON UPDATE clauses.
codeGenOnActionFunc :: CodeGenerator dbVendor -> OnAction dbVendor -> Doc
codeGenOnActionFunc codeGenerator (OnDelete action) =    "ON DELETE "
                                             <> _codeGenAction codeGenerator action
codeGenOnActionFunc codeGenerator (OnUpdate action) =    "ON UPDATE "
                                             <> _codeGenAction codeGenerator action

-- | Generate code for a table constraint.
codeGenTableConstFunc :: CodeGenerator dbVendor -> TableConstraint dbVendor -> Doc
codeGenTableConstFunc codeGenerator table = hsep $ catMaybes
    [ fmap  codeGenName                    $ table^.tableConstraintName
    , Just $ _codeGenTableConstType codeGenerator $ table^.tableConstraintType
    , fmap (_codeGenConstTiming codeGenerator)    $ table^.tableConstraintTiming
    ]
    where
        codeGenName name = "CONSTRAINT " <> _quoteElem codeGenerator name <> empty

-- | Generate code for a table constraint type.
codeGenTableConstTypeFunc :: CodeGenerator dbVendor -> TableConstraintType dbVendor -> Doc
codeGenTableConstTypeFunc codeGenerator cond =
    case cond of
        TCCheck condition -> hsep
            [ "CHECK"
            , parens $ _codeGenExpr codeGenerator $ ExprWrap condition
            ]

        TCForeignKey cols clause -> hsep
            [ "FOREIGN KEY"
            , parens $ codeGenCols cols
            , _codeGenFk codeGenerator clause
            ]

        TCPrimaryKey cols -> hsep
            [ "PRIMARY KEY"
            , parens $ codeGenCols cols
            ]

        TCUnique cols -> hsep
            [ "UNIQUE"
            , parens $ codeGenCols cols
            ]
    where
        codeGenCols cols = csep $ map (_codeGenCol codeGenerator) cols

-- | Generate code for a DELETE statement.
codeGenDeleteFunc :: CodeGenerator dbVendor -> DeleteWrap dbVendor -> Doc
codeGenDeleteFunc codeGenerator (DeleteWrap statement) =
        "DELETE FROM"
    <+> (_codeGenTableName codeGenerator) (statement ^. deleteTable)
    <$> codeGenM (_codeGenWhere codeGenerator) (statement ^. deleteWhere)
    <$> codeGenM (_codeGenReturning codeGenerator) returningClause
    where
        returningClause = fmap ReturningWrap (statement ^. deleteReturning)

-- | Generate code for a DROP TABLE statement.
codeGenDropFunc :: CodeGenerator dbVendor -> Drop dbVendor -> Doc
codeGenDropFunc codeGenerator dropClause =
    "DROP" <+>
    case dropClause of
        DropTable ifExists stmt -> hsep
            [ "TABLE"
            ,  exists ifExists
            ,  _codeGenTableName codeGenerator stmt
            ]
        DropView ifExists stmt -> hsep
            [ "VIEW"
            , exists ifExists
            , _codeGenView codeGenerator stmt
            ]
    where
        exists x = if x then "IF EXISTS" else empty

-- | Generate code for an INSERT statement.
codeGenInsertFunc :: CodeGenerator dbVendor -> InsertWrap dbVendor -> Doc
codeGenInsertFunc codeGenerator (InsertWrap insert) =
        "INSERT INTO"
    <+> (_quoteElem codeGenerator) (insert ^. insertTable.tableName)
    <+> (_codeGenInsertAssign codeGenerator) (insert ^. insertAssign)
    <$> codeGenM (_codeGenReturning codeGenerator) returningClause
    where
        returningClause = fmap ReturningWrap (insert ^. insertReturning)

-- | Generate code for INSERT assignments.
codeGenInsertAssignFunc :: CodeGenerator dbVendor -> [Assignment dbVendor] -> Doc
codeGenInsertAssignFunc codeGenerator assigns =
         "("
    <$$> indent 2 (vsep $ punctuate comma cols)
    <>   ")"
    <$>  "VALUES"
    <+>  "("
    <$$> indent 2 (vsep $ punctuate comma vals)
    <>   ")"
    where
        cols = map (_codeGenCol codeGenerator . getAssignCol) assigns
        vals = map (_codeGenExpr codeGenerator . getAssignExpr) assigns

-- | Generate code for a SELECT query.
codeGenSelectFunc :: CodeGenerator dbVendor -> SelectWrap dbVendor -> Doc
codeGenSelectFunc codeGenerator (SelectWrap (Combined combination queries)) =
    vsep $ intersperse combinator $ map encapsulate queries
    where
        combinator = _codeGenCombination codeGenerator combination
        codeGenQuery = _codeGenSelect codeGenerator . SelectWrap
        encapsulate q@(Single _) = codeGenQuery q
        encapsulate q@(_)        = parens $ codeGenQuery q

codeGenSelectFunc codeGenerator (SelectWrap (Single select)) =
        "SELECT"
    <+> (_codeGenSelectType codeGenerator (select^.selectType))
    $+> (_codeGenSelection codeGenerator  (SelectionWrap $ select^.selectCols))
    <$> codeGenM (_codeGenFrom codeGenerator)    (select^.selectFrom)
    <$> codeGenM (_codeGenWhere codeGenerator)   (select^.selectWhere)
    <$> codeGenM (_codeGenGroupBy codeGenerator) (select^.selectGroupBy)
    <$> codeGenM (_codeGenHaving codeGenerator)  (select^.selectHaving)
    <$> codeGenM (_codeGenOrderBy codeGenerator) (select^.selectOrderBy)
    <$> codeGenM (\(Limit v)  -> "LIMIT"  <+> int v) (select^.selectLimit)
    <+> codeGenM (\(Offset v) -> "OFFSET" <+> int v) (select^.selectOffset)
    where

        -- Columns flow in the SELECT clause.
        infixl 4 $+> -- Infix 5 is important to get things properly aligned!
        ($+>) = if (length $ select ^. selectCols ^. _Selection) > 1
                then (<$>)
                else (<+>)

{-|
Generate code for the columns of a SELECT clause.

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
codeGenSelectionFunc :: CodeGenerator dbVendor -> SelectionWrap dbVendor -> Doc
codeGenSelectionFunc codeGenerator (SelectionWrap (Selection cols))
    | nb == 0   = "*"
    | nb > 1    = indent 2 $ vsep $ mapButLast com $ map codeGen cols
    | otherwise = codeGen (head cols)
    where
        nb = length cols
        com x = x <> comma
        codeGen = (_codeGenColRef codeGenerator)

-- | Generate code for queries' combination clause such as "UNION", "EXCEPT", etc.
codeGenCombinationFunc :: Combination dbVendor -> Doc
codeGenCombinationFunc combination =
    case combination of
        Except       -> "EXCEPT"
        ExceptAll    -> "EXCEPT ALL"
        Intersect    -> "INTERSECT"
        IntersectAll -> "INTERSECT ALL"
        Union        -> "UNION"
        UnionAll     -> "UNION ALL"

codeGenSelectTypeFunc :: CodeGenerator dbVendor -> SelectType dbVendor -> Doc
codeGenSelectTypeFunc _ All                    = empty
codeGenSelectTypeFunc _ Distinct               = "DISTINCT"
codeGenSelectTypeFunc codeGenerator (DistinctOn refs) = hsep
    [ "DISTINCT ON"
    , parens $ csep $ map (_codeGenColRefDef codeGenerator) refs
    ]

-- | Generate code for a SQL statement.
codeGenStmtFunc :: CodeGenerator dbVendor -> Statement dbVendor -> Doc
codeGenStmtFunc codeGenerator stmt =
    case stmt of
        CreateStmt  s  -> _codeGenCreate codeGenerator s
        DeleteStmt  s  -> _codeGenDelete codeGenerator s
        DropStmt    s  -> _codeGenDrop   codeGenerator s
        InsertStmt  s  -> _codeGenInsert codeGenerator s
        SelectStmt  s  -> _codeGenSelect codeGenerator s
        UpdateStmt  s  -> _codeGenUpdate codeGenerator s
        Statements  xs -> vcat $ punctuate semi $ map (_codeGenStmt codeGenerator) xs

-- | Generate code for an UPDATE statement.
codeGenUpdateFunc :: CodeGenerator dbVendor -> UpdateWrap dbVendor -> Doc
codeGenUpdateFunc codeGenerator (UpdateWrap update) =
        "UPDATE"
    <+> _quoteElem codeGenerator (update^.updateTable.tableName)
    <$>  "SET"
    <+> csep (map (_codeGenAssgnmt codeGenerator) assignments)
    <$> codeGenM (_codeGenWhere codeGenerator) (update ^. updateWhere)
    <$> codeGenM (_codeGenReturning codeGenerator) returningClause
    where
        assignments = update ^. updateAssignments
        returningClause = fmap ReturningWrap (update ^. updateReturning)

{-|
Generate code for the assignment of an UPDATE statement.

Note: this function is located in the Query CodeGenerator because it is the only
one specific to the UPDATE statement. Thus, a dedicated UPDATE code generator
for this only purpose wouldn't make a lot of sense.
-}
codeGenAssgnmtFunc :: CodeGenerator dbVendor -> Assignment dbVendor -> Doc
codeGenAssgnmtFunc codeGenerator (Assignment col val) = hsep
    [ _codeGenCol codeGenerator $ ColWrap col
    , "="
    , _codeGenExpr codeGenerator $ ExprWrap val
    ]

-- | Generate code for the name of a column.
codeGenColFunc :: CodeGenerator dbVendor -> ColWrap dbVendor -> Doc
codeGenColFunc codeGenerator (ColWrap col) = _quoteElem codeGenerator $ col^.colName

-- | Generate code for a column definition.
codeGenColDefFunc :: CodeGenerator dbVendor -> ColDefWrap dbVendor -> Doc
codeGenColDefFunc codeGenerator (ColDefWrap colDef) = hsep
    [ maybe empty ((<>) "." . _codeGenTableRef codeGenerator) $ colDef^.colExprTableLabel
    , _codeGenCol codeGenerator $ ColWrap $ colDef^.colExpr
    ]

{-|
Generate code for a column reference using its alias if defined.

If the column reference is a column and belongs to a specified table reference,
then a qualified name will be returned (for example: "Table1"."col1").
-}
codeGenColRefDefFunc :: CodeGenerator dbVendor -> ColRefWrap dbVendor -> Doc
codeGenColRefDefFunc codeGenerator (ColRefWrap colRef) =
        maybe ifNothing (_quoteElem codeGenerator) (colRef^.colRefLabel)
    where
        ifNothing = _codeGenExpr codeGenerator $ ExprWrap $ colRef^.colRefExpr

-- | Define a column reference including its alias definition if specified.
codeGenColRefFunc :: CodeGenerator dbVendor -> ColRefWrap dbVendor -> Doc
codeGenColRefFunc codeGenerator (ColRefWrap colRef) =
       _codeGenExpr codeGenerator (ExprWrap $ colRef^.colRefExpr)
    <+> maybe empty ((<+>) "AS" . _quoteElem codeGenerator) (colRef^.colRefLabel)

-- | Generate code for a SQL expression.
codeGenExprFunc :: CodeGenerator dbVendor -> ExprWrap dbVendor -> Doc
codeGenExprFunc codeGenerator (ExprWrap expr) =
    case expr of
        ColExpr (ColDef col l) -> hcat
            [ maybe
                  ""
                  (flip (<>) "." . _quoteElem codeGenerator . getTableRefName)
                  l
            , _codeGenCol codeGenerator $ ColWrap col
            ]
        And c1 c2 p -> pCond "AND" (ExprWrap c1) (ExprWrap c2) p
        Or  c1 c2 p -> pCond "OR"  (ExprWrap c1) (ExprWrap c2) p
        SelectExpr s -> parens $ align $ _codeGenSelect codeGenerator $ SelectWrap s

        Value  val -> _codeGenValue codeGenerator $ ValueWrap val
        Values vals ->
            parens $ csep $ map (_codeGenValue codeGenerator . ValueWrap) vals

        -- Boolean Functions.
        Between ref lower higher ->
            codeGenBetweens
                True
                (ColRefWrap ref)
                (ColRefWrap lower)
                (ColRefWrap higher)
        Equal ref1 ref2 ->
            codeGenInfix "=" (ColRefWrap ref1) (ColRefWrap ref2)
        Exists colRef ->
            "EXISTS" <+> _codeGenColRefDef codeGenerator (ColRefWrap colRef)
        GreaterThan ref1 ref2 ->
            codeGenInfix ">" (ColRefWrap ref1) (ColRefWrap ref2)
        GreaterThanOrEqTo ref1 ref2 ->
            codeGenInfix ">=" (ColRefWrap ref1) (ColRefWrap ref2)
        In                ref1 ref2 ->
            codeGenInfix "IN" (ColRefWrap ref1) (ColRefWrap ref2)
        IsDistinctFrom    ref1 ref2 ->
            codeGenInfix "IS DISTINCT FROM" (ColRefWrap ref1) (ColRefWrap ref2)
        IsFalse e -> codeGenIs (ColRefWrap e) "FALSE"
        IsNotDistinctFrom ref1 ref2 -> codeGenInfix
                                            "IS NOT DISTINCT FROM"
                                            (ColRefWrap ref1)
                                            (ColRefWrap ref2)
        IsNotFalse        e      -> codeGenIs (ColRefWrap e) "NOT FALSE"
        IsNotNull         e      -> codeGenIs (ColRefWrap e) "NOT NULL"
        IsNotTrue         e      -> codeGenIs (ColRefWrap e) "NOT TRUE"
        IsNotUnknown      e      -> codeGenIs (ColRefWrap e) "NOT UNKNOWN"
        IsNull            e      -> codeGenIs (ColRefWrap e) "NULL"
        IsTrue            e      -> codeGenIs (ColRefWrap e) "TRUE"
        IsUnknown         e      -> codeGenIs (ColRefWrap e) "UNKNOWN"
        Like              ref1 ref2 ->
            codeGenInfix "LIKE" (ColRefWrap ref1) (ColRefWrap ref2)
        NotBetween ref lower higher ->
            codeGenBetweens
                False
                (ColRefWrap ref)
                (ColRefWrap lower)
                (ColRefWrap higher)
        NotEqual ref1 ref2 ->
            codeGenInfix "<>" (ColRefWrap ref1) (ColRefWrap ref2)
        NotIn ref1 ref2 ->
            codeGenInfix "NOT IN" (ColRefWrap ref1) (ColRefWrap ref2)
        SmallerThan ref1 ref2 ->
            codeGenInfix "<" (ColRefWrap ref1) (ColRefWrap ref2)
        SmallerThanOrEqTo ref1 ref2 ->
            codeGenInfix "<=" (ColRefWrap ref1) (ColRefWrap ref2)

        -- Operators.
        Add left right ->
            codeGenOp "+" (ColRefWrap left) (ColRefWrap right)
        BitAnd left right -> codeGenOp "&" (ColRefWrap left) (ColRefWrap right)
        BitOr left right -> codeGenOp "|" (ColRefWrap left) (ColRefWrap right)
        BitShiftLeft  left right ->
            codeGenOp "<<" (ColRefWrap left) (ColRefWrap right)
        BitShiftRight left right ->
            codeGenOp ">>" (ColRefWrap left) (ColRefWrap right)
        Divide left right ->
            codeGenOp "/" (ColRefWrap left) (ColRefWrap right)
        Modulo left right ->
            codeGenOp "%" (ColRefWrap left) (ColRefWrap right)
        Multiply left right ->
            codeGenOp "*" (ColRefWrap left) (ColRefWrap right)
        Substract left right ->
            codeGenOp "-" (ColRefWrap left) (ColRefWrap right)

        -- Functions.
        Count       e -> makeExpr "COUNT" (ColRefWrap e)
        CurrentDate   -> "CURRENT_DATE"
        Joker         -> "*"
        Max         e -> makeExpr "MAX" (ColRefWrap e)
        Min         e -> makeExpr "MIN" (ColRefWrap e)
        Random        -> "random()"
        Sum         e -> makeExpr "SUM" (ColRefWrap e)
        Trim        e -> makeExpr "TRIM" (ColRefWrap e)

        -- Utils
        LastInsertId -> "lastval()"

        -- MariaDB functions.
        CalcFoundRows -> error $
               "SQL_CALC_FOUND_ROWS is specific to MariaDB."
            ++ "Use the MariaDB code generator."
        FoundRows     -> error
              "FOUND_ROWS is specific to MariaDB. Use the MariaDB code generator."

    where
        pCond name c1 c2 p =
                if p then text "(" else empty
            <>  (_codeGenExpr codeGenerator) c1
            <$> text name <+> (_codeGenExpr codeGenerator) c2
            <>  if p then text ")" else empty

        codeGenBetweens func colRef lower higher = hsep
            [ _codeGenColRefDef codeGenerator colRef
            , if func then empty else "NOT"
            , "BETWEEN"
            , _codeGenColRefDef codeGenerator lower
            , "AND"
            , _codeGenColRefDef codeGenerator higher
            ]

        codeGenInfix name colRef1 colRef2 = hsep
            [ _codeGenColRefDef codeGenerator colRef1
            , name
            , _codeGenColRefDef codeGenerator colRef2
            ]

        -- Generate code for an operator.
        codeGenOp name colRef1 colRef2 =
            hsep [codeGenPart colRef1, name, codeGenPart colRef2]
            where
                codeGenPart c@(ColRefWrap cRef) =
                    if isOperator $ ExprWrap $ cRef^.colRefExpr
                    then parens $ _codeGenColRefDef codeGenerator c
                    else  _codeGenColRefDef codeGenerator c

        -- Return True if an expression is an operator.
        isOperator :: ExprWrap dbVendor -> Bool
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

        codeGenIs colRef t = hsep
            [ _codeGenColRefDef codeGenerator colRef
            , "IS"
            , t
            ]

        makeExpr str colRef =
            str <> ref
            where
                ref = let cRef = renderRaw $ _codeGenColRefDef codeGenerator colRef in
                      if T.head cRef == '(' && T.last cRef == ')'
                      then text cRef
                      else parens $ text cRef

-- | Generate code for a FROM clause.
codeGenFromFunc :: CodeGenerator dbVendor -> From dbVendor -> Doc
codeGenFromFunc codeGenerator (From [tableRef]) =
    "FROM" <+> _codeGenTableRef codeGenerator tableRef

codeGenFromFunc codeGenerator (From tableReferences) =
    "FROM" <$> indent 2 (vsep $ mapButLast (\x -> x <> comma) codeGendRefs)
    where
        codeGendRefs = map (_codeGenTableRef codeGenerator) tableReferences

{-|
Generate code for a GROUP BY clause.

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
codeGenGroupByFunc :: CodeGenerator dbVendor -> GroupBy dbVendor -> Doc
codeGenGroupByFunc codeGenerator clause =
    case clause of
        GroupBy [colRef] -> gb <+> codeGen colRef
        GroupBy colRefs  ->
                gb
            <$> indent 2 (vsep $ mapButLast (\x -> x <> comma) $ map codeGen colRefs)
    where
        gb = "GROUP BY"
        codeGen = _codeGenColRefDef codeGenerator

-- | Generate code for a HAVING clause.
-- TODO: correct this: should generate code for the global expression!!!
codeGenHavingFunc :: CodeGenerator dbVendor -> Having dbVendor -> Doc
codeGenHavingFunc codeGenerator havingClause =
    let
        e = getHavingExpr havingClause
        args = case e of
            ExprWrap(And _ _ _) -> ((<$>), indent 2)
            ExprWrap(Or  _ _ _) -> ((<$>), indent 2)
            _         -> ((<+>), id)
    in (fst args) "HAVING" $ snd args $ _codeGenExpr codeGenerator $ e

-- | Generate code for joins.
codeGenJoinFunc :: CodeGenerator dbVendor -> Join dbVendor -> Doc
codeGenJoinFunc codeGenerator join =
    case join of
        JoinCol joinType table1 table2 clause ->
            pJoin (_codeGenJoinTCol codeGenerator joinType) table1 table2 (Just clause)

        JoinTable joinType table1 table2 ->
            pJoin (_codeGenJoinTTable codeGenerator joinType) table1 table2 Nothing
    where
        -- Common part between column and table joins.
        pJoin joinType tableRef1 tableRef2 clause =
                _codeGenTableRef codeGenerator tableRef1
            <$> (joinType <+> _codeGenTableRef codeGenerator tableRef2)
            <$> fromMaybe empty (fmap (_codeGenJoinClause codeGenerator) clause)

-- | Generate code for an ORDER BY clause.
codeGenOrderByFunc :: CodeGenerator dbVendor -> OrderBy dbVendor -> Doc
codeGenOrderByFunc codeGenerator clause =
    getResult (clause^.orderBySortSpecList)
    where
      getResult []  = empty
      getResult [x] = "ORDER BY" <+> codeGen x
      getResult xs  =     "ORDER BY"
                      <$> (indent 2 $ vsep $ punctuate comma $ map codeGen xs)
      codeGen = _codeGenSortRef codeGenerator

-- | Generate code for the NULLS FIRST or NULLS LAST of a the sorting clause.
codeGenSortNullFunc :: SortNulls dbVendor -> Doc
codeGenSortNullFunc NullsFirst = "NULLS FIRST"
codeGenSortNullFunc NullsLast  = "NULLS LAST"

-- | Generate code for the ASC or DESC clauses.
codeGenSortOrderFunc :: SortOrder dbVendor -> Doc
codeGenSortOrderFunc Asc  = "ASC"
codeGenSortOrderFunc Desc = "DESC"

-- | Generate code for a sort reference.
codeGenSortRefFunc :: CodeGenerator dbVendor -> SortRef dbVendor -> Doc
codeGenSortRefFunc codeGenerator sortRef =
       (_codeGenColRefDef codeGenerator)    (sortRef^.sortRefColRef)
   <+> codeGenM (_codeGenSortOrder codeGenerator) (sortRef^.sortRefOrder)
   <+> codeGenM (_codeGenSortNull codeGenerator)  (sortRef^.sortRefNulls)

-- | Generate code for the name of a table.
codeGenTableNameFunc :: CodeGenerator dbVendor -> Table dbVendor -> Doc
codeGenTableNameFunc codeGenerator table = _quoteElem codeGenerator $ table^.tableName

-- | Generate code for a table reference for the use in a FROM clause.
codeGenTableRefFunc :: CodeGenerator dbVendor -> TableRef dbVendor -> Doc
codeGenTableRefFunc codeGenerator join =
    case join of
        JoinRef ref alias -> hsep
            [  maybe empty (const "(") alias
            <> _codeGenJoin codeGenerator ref
            <> maybe empty (const ")") alias
            ,  maybe empty pAlias alias
            ]
        TableRef table alias ->
               _codeGenTableName codeGenerator table
            <+> fromMaybe empty (fmap pAlias alias)
        SelectRef select alias ->
            parens (align $ _codeGenSelect codeGenerator select) <+> pAlias alias
        LateralRef select alias -> hsep
            [ "LATERAL"
            , parens $ _codeGenSelect codeGenerator select
            , pAlias alias
            ]
    where
        pAlias alias = _codeGenTableRefAs codeGenerator alias

-- | Generate code for a table alias.
codeGenTableRefAsFunc :: CodeGenerator dbVendor -> TableRefAs dbVendor -> Doc
codeGenTableRefAsFunc codeGenerator alias =
    "AS" <+> _quoteElem codeGenerator (alias^.tableRefAsName)

-- | Generate code for an input values.
codeGenValueFunc :: CodeGenerator dbVendor -> ValueWrap dbVendor -> Doc
codeGenValueFunc codeGenerator (ValueWrap val) =
    case val of
        BoolVal True     -> "TRUE"
        BoolVal False    -> "FALSE"
        DefaultVal       -> "DEFAULT"
        DoubleVal      x -> double x
        FloatVal       x -> float x
        IntVal         x -> int x
        NumericVal     x -> text $ T.pack $ show x
        StringVal      x -> _quoteVal codeGenerator x
        GenQVal        x -> _quoteVal codeGenerator x
        GenVal         x -> text $ T.pack $ show x
        NullVal          -> "NULL"
        _                -> "?"

-- | Generate code for a WHERE clause.
codeGenWhereFunc :: CodeGenerator dbVendor -> Where dbVendor -> Doc
codeGenWhereFunc codeGenerator (Where e) =
    let
        args = case e of
            And _ _ _ -> ((<$>), indent 2)
            Or  _ _ _ -> ((<$>), indent 2)
            _         -> ((<+>), id)
    in (fst args) "WHERE" $ snd args $ _codeGenExpr codeGenerator $ ExprWrap e

codeGenReturningFunc :: CodeGenerator dbVendor -> ReturningWrap dbVendor -> Doc
codeGenReturningFunc codeGenerator (ReturningWrap (Returning sel)) =
    "RETURNING " <> (_codeGenSelection codeGenerator $ SelectionWrap sel)

-- | Generate code for the ON or USING clause of a JOIN.
codeGenJoinClauseFunc :: CodeGenerator dbVendor -> JoinClause dbVendor -> Doc
codeGenJoinClauseFunc codeGenerator jClause =
    case jClause of
        JoinClauseOn predicate ->
            "ON" <+> encapsulate predicate (makeCond predicate)
        JoinClauseUsing cols -> hsep
                [ "USING"
                , parens $ csep $ map (_codeGenCol codeGenerator) cols
                ]
     where
        makeCond predicate = _codeGenExpr codeGenerator $ ExprWrap predicate

        encapsulate :: Expression Bool dbVendor -> Doc -> Doc
        encapsulate (And _ _ _) str  = parens str
        encapsulate (Or  _ _ _) str  = parens str
        encapsulate _           str = str

-- | Generate SQL for a join on a column.
codeGenJoinTColFunc :: JoinTypeCol dbVendor -> Doc
codeGenJoinTColFunc join =
    case join of
        FullJoin  -> "FULL JOIN"
        LeftJoin  -> "LEFT JOIN"
        InnerJoin -> "INNER JOIN"
        RightJoin -> "RIGHT JOIN"

-- | Generate SQL for a join on a table.
codeGenJoinTTableFunc :: JoinTypeTable dbVendor -> Doc
codeGenJoinTTableFunc joinType =
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
genQuote quote t = text $ T.pack quotedString
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
renderRaw :: Doc -> T.Text
renderRaw doc =  displayT $ renderOneLine doc

renderP :: Doc -> T.Text
renderP doc =  displayT $ renderPretty 1.0 72 doc

{-|
Separate a list of 'Doc' with a comma and a space.
> csep ["a", "b", "c"] == "a, b, c"
-}
csep :: [Doc] -> Doc
csep = hsep . punctuate comma
