{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/Parser/Query.hs
Description : Implementation of the SQL query parsers.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL query parsers, which includes parser for the
following statements.
- SELECT
- DELETE
- UPDATE
-}
module Hedsql.Common.Parser.Queries
    (
      QueryParser(QueryParser)
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
    
    , parseColFunc
    , parseColRefFunc
    , parseColRefDefFunc
    , parseConditionFunc
    , parseDeleteFunc
    , parseDropTableFunc
    , parseDropViewFunc
    , parseExprFunc
    , parseFromFunc
    , parseJoinFunc
    , parseGroupByFunc
    , parseHavingFunc
    , parseOrderByFunc
    , parseSelectFunc -- The SELECT function implementation is located here.
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

import Hedsql.Common.DataStructure.Base
import Hedsql.Common.Parser.Interface
import Hedsql.Helpers.Patterns

import Control.Lens
import Data.Maybe


-- Definition of the parsers interfaces.

{-|
Interface of the query parser.
-}
data QueryParser a = QueryParser
    {
      _parseCol        :: Column a        -> String
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
    {
      _parseJoinClause :: JoinClause a    -> String
    , _parseJoinTCol   :: JoinTypeCol a   -> String
    , _parseJoinTTable :: JoinTypeTable a -> String
    }

makeLenses ''JoinParser

-- Query parser.

-- Private functions.

parseIf :: Bool -> String -> String
parseIf True a  = a
parseIf False _ = ""

{-|
Apply parenthesis to a list of strings and use the specified separator to
delimit its elements.
-}
parenthesis :: String -> [String] -> String
parenthesis separator = getPatternFromList "(" ")" separator

parseMaybe :: (a -> String) -> Maybe a -> String
parseMaybe pFunc (Just a) = " " ++ pFunc a
parseMaybe _     Nothing  = ""
  
-- Implementation of the functions.

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
    ++  parseMaybe ((++) " AS") (fmap (parser^.quoteElem) (colRef^.colRefLabel))
        
-- | Parse a condition.
parseConditionFunc :: QueryParser a -> Condition a -> String
parseConditionFunc parser condition =
    case condition of
        FuncCond func  -> parser^.parseFuncBool $ func
        And conds      -> pCond "AND" conds
        Or  conds      -> pCond "OR"  conds
    where
        cs = map (parser^.parseCondition)
        pCond name conds =
            getPatternFromList "(" (" " ++ name ++ " ") ")" $ cs conds
            
-- | Parse a DELETE statement.
parseDeleteFunc :: QueryParser a -> Delete a -> String
parseDeleteFunc parser statement =
       "DELETE "
    ++            (parser^.parseTableName) (statement^.deleteTable) 
    ++ parseMaybe (parser^.parseWhere)     (statement^.deleteWhere)

-- | Parse a DROP TABLE statement.
parseDropTableFunc :: QueryParser a -> DropTable a -> String
parseDropTableFunc parser statement =
        "Drop Table "
    ++  parseIf (statement^.dropTableIfExistsParam) "IF EXISTS"
    ++  (parser^.parseTableName) (statement^.dropTableTable)

-- | Parse a DROP VIEW statement.
parseDropViewFunc :: QueryParser a -> DropView a -> String
parseDropViewFunc parser statement =
    "Drop View " ++ (parser^.quoteElem $ statement^.dropViewName)

-- | Parse a SQL expression.
parseExprFunc :: QueryParser a -> Parser a -> Expression a -> String
parseExprFunc parser genParser expr =
    case expr of
        ColExpr col        -> parser^.parseCol $ col
        SelectExpr select  -> "(" ++ (genParser^.parseSelect) select ++ ")"   
        FuncExpr func      -> parser^.parseFunc $ func
        ValueExpr val      -> parser^.parseValue $ val
        ValueExprs vals    -> parenthesis ", " $ map (parser^.parseValue) vals

-- | Parse a FROM clause.
parseFromFunc :: QueryParser a -> From a -> String
parseFromFunc parser (From tableReferences) =
       foldl (++) "FROM " $ getReferences tableReferences
       where
           getReferences = map (parser^.parseTableRef)

-- | Parse a GROUP BY clause.
parseGroupByFunc :: QueryParser a -> GroupBy a -> String
parseGroupByFunc parser (GroupBy colRefs having) =
       getPatternFromList "GROUP BY " "" ", " parsedColRefs
    ++ parseMaybe (parser^.parseHaving) having
    where
        parsedColRefs = map (parser^.parseColRef) colRefs

-- | Parse a HAVING clause.
parseHavingFunc :: QueryParser a -> Having a -> String
parseHavingFunc parser (Having c) = "HAVING " ++ (parser^.parseCondition) c

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
        JoinClauseOn predicate -> " ON " ++ (parser^.parseCondition) predicate
        JoinClauseUsing cols   ->   getPatternFromList " USING(" ")" ", "
                                  $ map (parser^.parseCol) cols

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
       getPatternFromList "ORDER BY " "" ", " sortRefsParsed
    ++ parseMaybe show (clause^.partOrderByLimit)
    ++ parseMaybe show (clause^.partOrderByOffset)
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
                      (parser^.parseColRef)       (sortRef^.sortRefCol)
        ++ parseMaybe (parser^.parseSortOrder)    (sortRef^.sortRefOrder)
        ++ parseMaybe (parser^.parseSortNull)     (sortRef^.sortRefNulls)

-- | Parse a SELECT query.
parseSelectFunc :: QueryParser a -> Select a -> String
parseSelectFunc parser select =
      "SELECT"
    ++ parseMaybe  parseDistinct         (select^.selectType)
    ++             parseColRefs          (select^.selectColRef)
    ++ parseMaybe (parser^.parseFrom)    (select^.fromClause)
    ++ parseMaybe (parser^.parseWhere)   (select^.whereClause)
    ++ parseMaybe (parser^.parseGroupBy) (select^.groupByClause)
    ++ parseMaybe (parser^.parseOrderBy) (select^.orderByClause)
    where
        parseColRefs colRefs =
            getPatternFromList " " "" ", " $ map (parser^.parseColRef) colRefs
        pExprs exprs = map (parser^.parseExpr) exprs
        parseDistinct  All               = "All"
        parseDistinct  Distinct          = "DISTINCT "
        parseDistinct (DistinctOn exprs) =
            getPatternFromList "DISTINCT ON (" ") " ", " $ pExprs exprs
        
{-|
Parse the name of a table.
-}
parseTableNameFunc :: QueryParser a -> Table a -> String
parseTableNameFunc parser table = parser^.quoteElem $ table^.tableName

-- | Parse a table reference for the use in a FROM clause.
parseTableRefFunc :: Parser a -> QueryParser a -> TableRef a -> String
parseTableRefFunc p parser join =
    case join of
        TableJoinRef    ref    alias -> concat [
                                                 "("
                                               , parser^.parseJoin $ ref
                                               , ")"
                                               , parseMaybe pAlias alias
                                               ]
        TableTableRef   table  alias -> concat [
                                                 parser^.parseTableName $ table
                                               , parseMaybe pAlias alias
                                               ]
        SelectTableRef  select alias -> concat [
                                                 "("
                                               , p^.parseSelect $ select
                                               , ")"
                                               , pAlias alias
                                               ]
        LateralTableRef select alias -> concat [
                                                 "LATERAL ("
                                               , p^.parseSelect $ select
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

-- | Parse a WHERE clause.  
parseWhereFunc :: QueryParser a -> Where a -> String
parseWhereFunc parser (Where condition) =
    "WHERE " ++ (parser^.parseCondition) condition