{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Database/Hedsql/Common/Parser/TableManipulations.hs
Description : Implementation of the SQL parser for table manipulation.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL parser for table manipulation statements such as
CREATE TABLE and CREATE VIEW.
-}
module Database.Hedsql.Common.Parser.TableManipulations
    ( -- Interface.
      TableParser(TableParser)
    , parseAction
    , parseCol
    , parseColCreate
    , parseColConst
    , parseColConstType
    , parseConstTiming
    , parseConstTimingCheck
    , parseConstTimingType
    , parseCreateTable
    , parseCreateView
    , parseDataType
    , parseExpr
    , parseFkClause
    , parseMatchClause
    , parseOnAction
    , parseTableConst
    , parseTableConstType
    , parseSelect
    , parseTable
    , quoteElem
    
      -- Generic functions implementation.
    , parseActionFunc
    , parseColCreateFunc
    , parseColConstFunc
    , parseColConstTypeFunc
    , parseConstTimingFunc
    , parseConstTimingCheckFunc
    , parseConstTimingTypeFunc
    , parseCreateTableFunc
    , parseCreateViewFunc
    , parseDataTypeFunc
    , parseFkClauseFunc
    , parseMatchClauseFunc
    , parseOnActionFunc
    , parseTableConstFunc
    , parseTableConstTypeFunc
    ) where
    
import Database.Hedsql.Common.DataStructure

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (catMaybes)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- Parsers interfaces
----------------------------------------

{-|
Function parser interface.

Parse all functions which are not returning a boolean value.
Those functions have their own parser.
-}
data TableParser a = TableParser
    {
      _parseAction           :: SqlAction             a      -> String
    , _parseCol              :: ColWrap               a      -> String
    , _parseColCreate        :: ColWrap               a      -> String
    , _parseColConst         :: ColConstraint         a      -> String
    , _parseColConstType     :: ColConstraintType     a      -> String
    , _parseConstTiming      :: ConstraintTiming      a      -> String
    , _parseConstTimingCheck :: ConstraintTimingCheck a      -> String
    , _parseConstTimingType  :: ConstraintTimingType  a      -> String
    , _parseCreateTable      :: Table                 a      -> String
    , _parseCreateView       :: CreateView            a      -> String
    , _parseDataType         :: DataTypeWrap          a      -> String
    , _parseExpr             :: ExprWrap              a      -> String
    , _parseFkClause         :: ForeignKeyClause      a      -> String
    , _parseMatchClause      :: Match                 a      -> String
    , _parseOnAction         :: OnAction              a      -> String
    , _parseTableConst       :: TableConstraint       a      -> String
    , _parseTableConstType   :: TableConstraintType   a      -> String
    , _parseSelect           :: SelectWrap            a      -> String
    , _parseTable            :: Table                 a      -> String
    
    -- Helper functions.
    , _quoteElem :: String -> String
    }

makeLenses ''TableParser

-- Helpers functions.

-- Generic implementation of the functions.

-- | Parse a CASCADE or RESTRICT action.
parseActionFunc :: SqlAction a -> String
parseActionFunc Cascade    = "CASCADE"
parseActionFunc NoAction   = ""
parseActionFunc Restrict   = "RESTRICT"
parseActionFunc SetDefault = "SET DEFAULT"
parseActionFunc SetNull    = "SET NULL"

-- | Parse a column which can be used for a CREATE statement.
parseColCreateFunc :: TableParser a -> ColWrap a -> String
parseColCreateFunc parser col = concat
    [ _quoteElem parser             $ col^.colWrapName
    , " " ++ (parser^.parseDataType) (col^.colWrapDataType)
    , consts                        $ col^.colWrapConstraints
    ]
    where
        consts []   = ""
        consts cols =
            " " ++ intercalate ", " (map (_parseColConst parser) cols)

-- | Parse a column constraint type.
parseColConstTypeFunc :: TableParser a -> ColConstraintType a -> String
parseColConstTypeFunc parser cst =
    case cst of
        (Check condition) -> 
            "CHECK (" ++ (parser^.parseExpr) (ExprWrap condition) ++ ")"
    
        (Default expr) ->
            "DEFAULT(" ++ (parser^.parseExpr) (ExprWrap expr) ++ ")"
        
        NotNull ->
            "NOT NULL"
        
        Null ->
            "NULL"
    
        (Primary isAuto)  ->
            "PRIMARY KEY" ++ auto isAuto
                where
                    auto True = " AUTOINCREMENT"
                    auto False = ""
        
        (Reference table col action) -> concat
            [ "REFERENCES "
            , (parser^.parseTable) table
            , "("
            , (_quoteElem parser) (col^.colWrapName)
            , ")"
            , makeAction action
            ]
            where
                makeAction (Just act) = " " ++ (parser^.parseOnAction) act
                makeAction Nothing = ""
         
        Unique ->
            "UNIQUE"

-- | Parse a column constraint.
parseColConstFunc :: TableParser a -> ColConstraint a -> String
parseColConstFunc parser colConstraint = 
        parseName                  (colConstraint^.colConstraintName)
    ++ (parser^.parseColConstType) (colConstraint^.colConstraintType)
        where
            parseName (Just name) =    "CONSTRAINT " 
                                    ++ (_quoteElem parser) name
                                    ++ " "
            parseName  Nothing    = ""
        
-- | Parse a timing constraint.
parseConstTimingFunc :: TableParser a -> ConstraintTiming  a -> String
parseConstTimingFunc parser timing =
       (_parseConstTimingType parser)  (timing^.constraintTimingType)
    ++ " "
    ++ (parser^.parseConstTimingCheck) (timing^.constraintTimingCheck)

-- | Parse a timing type constraint; "DEFERABLE" or "NOT DEFERABLE".    
parseConstTimingTypeFunc :: ConstraintTimingType a -> String
parseConstTimingTypeFunc Deferable    = "DEFERABLE"
parseConstTimingTypeFunc NotDeferable = "NOT DEFERABLE"

{-|
Parse a timing check constraint; "INITIALLY IMMEDIATE" or "INITIALLY DEFERRED"
-}
parseConstTimingCheckFunc :: ConstraintTimingCheck a -> String
parseConstTimingCheckFunc InitiallyImmediate = "INITIALLY IMMEDIATE"
parseConstTimingCheckFunc InitiallyDeferred  = "INITIALLY DEFERRED"

-- | Parse a CREATE TABLE statement.   
parseCreateTableFunc :: TableParser a -> Table a -> String
parseCreateTableFunc parser stmt = concat
    [ "CREATE TABLE "
    , parser^.parseTable $ stmt
    , " ("
    , intercalate ", " $ map (parser^.parseColCreate) (stmt^.tableCols)
    , constraints (stmt^.tableConsts)
    , ")"
    ]
    where
        constraints [] = ""
        constraints xs =
            ", " ++ intercalate ", " (map (parser^.parseTableConst) xs)
        

-- | Create a CREATE VIEW statement.
parseCreateViewFunc :: TableParser a -> CreateView a -> String
parseCreateViewFunc parser stmt = concat
    [ "CREATE VIEW "
    , _quoteElem parser $ stmt^.viewName
    , " AS "
    , parser^.parseSelect $ stmt^.viewSelect
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
parseFkClauseFunc :: TableParser a -> ForeignKeyClause a -> String
parseFkClauseFunc parser fk =
    concat
        [ (parser^.parseTable) (fk^.foreignKeyClauseTable)
        , " ("
        , intercalate ", " $ map (parser^.parseCol) (fk^.foreignKeyClauseCols)
        , ")"
        , makeMatch  $ fk^.foreignKeyMatch
        , makeAction $ _foreignKeyClauseAction fk
        ]
        where
            makeMatch (Just match) = " " ++ (parser^.parseMatchClause) match
            makeMatch  Nothing     = ""
            makeAction (Just action) = " " ++ (parser^.parseOnAction) action
            makeAction  Nothing      = ""

-- | Parse a MATCH clause.
parseMatchClauseFunc :: Match a -> String
parseMatchClauseFunc Full    = "FULL"
parseMatchClauseFunc Partial = "PARTIAL"
parseMatchClauseFunc Simple  = "SIMPLE"

-- | Parse ON DELETE or ON UPDATE clauses.
parseOnActionFunc :: TableParser a -> OnAction a -> String
parseOnActionFunc parser (OnDelete action) =    "ON DELETE "
                                             ++ (parser^.parseAction) action
parseOnActionFunc parser (OnUpdate action) =    "ON UPDATE "
                                             ++ (parser^.parseAction) action
                                             
-- | Parse a table constraint.
parseTableConstFunc :: TableParser a -> TableConstraint a -> String
parseTableConstFunc parser table = concat $ catMaybes
    [ fmap  parseName                    $ table^.tableConstraintName
    , Just $ parser^.parseTableConstType $ table^.tableConstraintConstraint
    , fmap  parseTiming                  $ table^.tableConstraintTiming
    ]
    where
        -- TODO: check if constraints name can be quoted.
        parseName name = "CONSTRAINT " ++ (_quoteElem parser) name ++ " "
        parseTiming timing = " " ++ (_parseConstTiming parser) timing
        
-- | Parse a table constraint type.
parseTableConstTypeFunc :: TableParser a -> TableConstraintType a -> String
parseTableConstTypeFunc parser cond =
    case cond of
        (TableConstraintCheck condition) -> concat
            [ "CHECK ("
            , (parser^.parseExpr) (ExprWrap condition)
            , ")"
            ]
    
        (ForeignKey cols clause) -> concat
            [ "FOREIGN KEY ("
            , parseCols cols
            , ")" 
            , _parseFkClause parser $ clause
            ]
    
        (TableConstraintPrimaryKey cols) -> concat
            [ "PRIMARY KEY ("
            , parseCols cols
            , ")"
            ]
        
        (TableConstraintUnique cols) -> concat
            [ "UNIQUE ("
            , parseCols cols
            , ")"
            ]
    where
        parseCols cols = intercalate ", " $ map (parser^.parseCol) cols   