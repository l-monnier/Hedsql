{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/Parser/TableManipulations.hs
Description : Implementation of the SQL parser for table manipulation.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL parser for table manipulation statements such as
CREATE TABLE and CREATE VIEW.
-}
module Hedsql.Common.Parser.TableManipulations
    ( -- Interface.
      TableParser(TableParser)
    , parseAction
    , parseCol
    , parseColCreate
    , parseColConst
    , parseColConstType
    , parseCondition
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

import Hedsql.Common.DataStructure

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (catMaybes)

-- Definition of the parsers interfaces.

{-|
Function parser interface.

Parse all functions which are not returning a boolean value.
Those functions have their own parser.
-}
data TableParser a = TableParser
    {
      _parseAction           :: SqlAction             a -> String
    , _parseCol              :: Column                a -> String
    , _parseColCreate        :: Column                a -> String
    , _parseColConst         :: ColConstraint         a -> String
    , _parseColConstType     :: ColConstraintType     a -> String
    , _parseCondition        :: Condition             a -> String
    , _parseConstTiming      :: ConstraintTiming      a -> String
    , _parseConstTimingCheck :: ConstraintTimingCheck a -> String
    , _parseConstTimingType  :: ConstraintTimingType  a -> String
    , _parseCreateTable      :: CreateTable           a -> String
    , _parseCreateView       :: CreateView            a -> String
    , _parseDataType         :: SqlDataType           a -> String
    , _parseExpr             :: Expression            a -> String
    , _parseFkClause         :: ForeignKeyClause      a -> String
    , _parseMatchClause      :: Match                 a -> String
    , _parseOnAction         :: OnAction              a -> String
    , _parseTableConst       :: TableConstraint       a -> String
    , _parseTableConstType   :: TableConstraintType   a -> String
    , _parseSelect           :: Select                a -> String
    , _parseTable            :: Table                 a -> String
    
    -- Helper functions.
    , _quoteElem :: String -> String
    }
    
makeLenses ''TableParser

-- Helpers functions.

-- Generic implementation of the functions.

-- | Parse a CASCADE or RESTRICT action.
parseActionFunc :: SqlAction a -> String
parseActionFunc Cascade  = "CASCADE"
parseActionFunc Restrict = "RESTRICT"

-- | Parse a column which can be used for a CREATE statement.
parseColCreateFunc :: TableParser a -> Column a -> String
parseColCreateFunc parser col = concat $ catMaybes
    [ Just $ parser^.quoteElem                        $ col^.colName
    , fmap   (\x -> " " ++ (parser^.parseDataType) x) $ col^.colDataType
    , fmap   consts                                   $ col^.colConstraints
    ]
    where
        consts cols =
            " " ++ (intercalate ", " $ map (parser^.parseColConst) cols)

-- | Parse a column constraint type.
parseColConstTypeFunc :: TableParser a -> ColConstraintType a -> String
parseColConstTypeFunc parser const =
    case const of
        (Check condition)-> 
            "CHECK (" ++ (parser^.parseCondition) condition ++ ")"
    
        (Default expr)   -> "DEFAULT(" ++ (parser^.parseExpr) expr ++ ")"
        NotNull          -> "NOT NULL"
        Null             -> "NULL"
    
        (Primary isAuto)  ->
            "PRIMARY KEY" ++ auto isAuto
                where
                    auto True = " AUTOINCREMENT"
                    auto False = ""
        
        (Reference table col action) -> concat
            [ "REFERENCES "
            , (parser^.parseTable) table
            , "("
            , (parser^.quoteElem) (col^.colName)
            , ")"
            , makeAction action
            ]
            where
                makeAction (Just action) = " " ++ (parser^.parseOnAction) action
                makeAction Nothing = ""
         
        Unique -> "UNIQUE"

-- | Parse a column constraint.
parseColConstFunc :: TableParser a -> ColConstraint a -> String
parseColConstFunc parser colConstraint = 
        parseName                  (colConstraint^.colConstraintName)
    ++ (parser^.parseColConstType) (colConstraint^.colConstraintType)
        where
            parseName (Just name) =    "CONSTRAINT " 
                                    ++ (parser^.quoteElem) name
                                    ++ " "
            parseName  Nothing    = ""
        
-- | Parse a timing constraint.
parseConstTimingFunc :: TableParser a -> ConstraintTiming  a -> String
parseConstTimingFunc parser timing =
       (parser^.parseConstTimingType)  (timing^.constraintTimingType)
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
parseCreateTableFunc :: TableParser a -> CreateTable a -> String
parseCreateTableFunc parser stmt = concat
    [ "CREATE TABLE "
    , parser^.parseTable $ stmt^.createTableTable
    , " ("
    , intercalate ", " $ map (parser^.parseColCreate) (stmt^.createTableCols)
    , constraints (stmt^.createTableConstraints)
    , ")"
    ]
    where
        constraints (Just consts) =
            ", " ++ (intercalate ", " $ map (parser^.parseTableConst) consts)
        constraints Nothing = ""

-- | Create a CREATE VIEW statement.
parseCreateViewFunc :: TableParser a -> CreateView a -> String
parseCreateViewFunc parser stmt = concat
    [ "CREATE VIEW "
    , parser^.quoteElem $ stmt^.viewName
    , " AS "
    , parser^.parseSelect $ stmt^.viewSelect
    ]

-- | Parse SQL data types.
parseDataTypeFunc :: SqlDataType a -> String
parseDataTypeFunc  Date            = "date"
parseDataTypeFunc (SqlChar lenght) = "char(" ++ show lenght ++ ")"
parseDataTypeFunc  SmallInt        = "smallint"
parseDataTypeFunc  Integer         = "integer"
parseDataTypeFunc  BigInt          = "bigint"
parseDataTypeFunc (Varchar max)    = "varchar(" ++ show max ++ ")"

-- | Parse a FOREIGN KEY clause.
parseFkClauseFunc :: TableParser a -> ForeignKeyClause a -> String
parseFkClauseFunc parser fk =
    concat
        [ (parser^.parseTable) (fk^.foreignKeyClauseTable)
        , " ("
        , intercalate ", " $ map (parser^.parseCol) (fk^.foreignKeyClauseCols)
        , ")"
        , makeMatch  $ fk^.foreignKeyMatch
        , makeAction $ fk^.foreignKeyClauseAction
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
        parseName name = "CONSTRAINT " ++ (parser^.quoteElem) name ++ " "
        parseTiming timing = " " ++ (parser^.parseConstTiming) timing
        
-- | Parse a table constraint type.
parseTableConstTypeFunc :: TableParser a -> TableConstraintType a -> String
parseTableConstTypeFunc parser cond =
    case cond of
        (TableConstraintCheck condition) -> 
                "CHECK "
             ++ (parser^.parseCondition) condition
    
        (ForeignKey cols clause) -> concat
            [ "FOREIGN KEY ("
            , parseCols cols
            , ")" 
            , parser^.parseFkClause $ clause
            ]
    
        (TableConstraintPrimaryKey cols) -> concat
            [ "PRIMARY KEY ("
            , parseCols cols
            , ")"
            ]
        
        (TableConstraintUnique cols) -> concat
            [ "UNIQUE ("
            , parseCols cols
            ]
    where
        parseCols cols = intercalate ", " $ map (parser^.parseCol) cols      