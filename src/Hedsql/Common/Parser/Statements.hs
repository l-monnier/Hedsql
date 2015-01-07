{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/Parser/Query.hs
Description : Implementation of the SQL query parsers.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL statements parser.
-}
module Hedsql.Common.Parser.Statements
    (
      _parseDelete
    , _parseDropTable
    , _parseDropView
    , _parseInsert
    , _parseSelect
    , _parseUpdate
    ) where

import Hedsql.Common.DataStructure
import Hedsql.Common.Parser.Interface
import Hedsql.Helpers.Patterns

import Control.Lens
import Data.Maybe


-- Definition of the parsers interfaces.

-- Private.
  
-- Implementation of the functions.
            
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