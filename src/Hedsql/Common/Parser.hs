{-|
Module      : Hedsql/Common/Parser.hs
Description : Generic SQL parser.
Copyright   : (c) Leonard Monnier2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Generic implementation of a SQL parser.

This generic implementation can then be modified to fit the needs of a specific
vendor such as PostgreSQL.

Typically, to get your own parser you will:
    1) get the generic query parser using genQueryParser and modify it;
    2) provide this modified version to the getGenParser function.
-}
module Hedsql.Common.Parser
    (
      module Hedsql.Common.Parser.BoolFunctions
    , module Hedsql.Common.Parser.Functions
    , module Hedsql.Common.Parser.Interface
    , module Hedsql.Common.Parser.Queries
    , genQueryParser
    , getGenBoolFuncParser
    , getGenFuncParser
    , getGenJoinParser
    , getParser
    , getQueryParser
    , getStmtParser
    ) where

import Hedsql.Common.Parser.BoolFunctions
import Hedsql.Common.Parser.Functions
import Hedsql.Common.Parser.Interface
import Hedsql.Common.Parser.Queries
import qualified Hedsql.Common.Parser.Quoter as Q

import Control.Lens

-- Private.

-- Public.

-- | Generic query parser.
genQueryParser :: QueryParser a
genQueryParser = getQueryParser genQueryParser (getGenFuncParser genQueryParser)

{-|
Return a query parser using the provided query parser.
-}
getQueryParser :: QueryParser a -> FuncParser a -> QueryParser a
getQueryParser queryParser funcParser = QueryParser
    (parseAssgnmtFunc queryParser)
    (parseColFunc queryParser)
    (parseColRefFunc queryParser)
    (parseColRefDefFunc queryParser)
    (parseConditionFunc queryParser)
    (parseFuncFunc funcParser)
    (parseFuncBoolFunc genBoolFuncParser)
    (parseExprFunc queryParser stmtParser)
    (parseFromFunc queryParser)
    (parseJoinFunc queryParser genJoinParser)
    (parseGroupByFunc queryParser)
    (parseHavingFunc queryParser)
    (parseOrderByFunc queryParser)
     parseSortNullFunc
    (parseSortRefFunc queryParser)
     parseSortOrderFunc
    (parseTableNameFunc queryParser)
    (parseTableRefFunc stmtParser queryParser)
    (parseTableRefAsFunc queryParser)
    (parseValueFunc queryParser)
    (parseWhereFunc queryParser)
    (Q.genQuoter ^. Q.quoteElem)
    (Q.genQuoter ^. Q.quoteVal)
    where
        genBoolFuncParser = getGenBoolFuncParser queryParser
        genJoinParser     = getGenJoinParser queryParser
        stmtParser        = getStmtParser queryParser

{-|
Get a generic boolean functions parser by providing its internal query
parser.
-}
getGenBoolFuncParser :: QueryParser a -> FuncBoolParser a
getGenBoolFuncParser queryParser = FuncBoolParser
    (parseBetweenFunc genBoolFuncParser)
    (parseEqualFunc genBoolFuncParser)
    (parseExistsFunc queryParser)
    (parseGreaterThanFunc genBoolFuncParser)
    (parseGreaterThanOrEqToFunc genBoolFuncParser)
    (parseInFunc genBoolFuncParser)
    (parseIsDistinctFromFunc genBoolFuncParser)
    (parseIsFalseFunc genBoolFuncParser)
    (parseIsNotDistinctFromFunc genBoolFuncParser)
    (parseIsNotFalseFunc genBoolFuncParser)
    (parseIsNotNullFunc genBoolFuncParser)
    (parseIsNotTrueFunc genBoolFuncParser)
    (parseIsNotUnknownFunc genBoolFuncParser)
    (parseIsNullFunc genBoolFuncParser)
    (parseIsTrueFunc genBoolFuncParser)
    (parseIsUnknownFunc genBoolFuncParser)
    (parseLikeFunc genBoolFuncParser)
    (parseNotBetweenFunc genBoolFuncParser)
    (parseNotEqualFunc genBoolFuncParser)
    (parseNotInFunc genBoolFuncParser)
    (parseSmallerThanFunc genBoolFuncParser)
    (parseSmallerThanOrEqToFunc genBoolFuncParser)
    (parseInfixFunc queryParser)
    (parseIsFunc queryParser)
    (parseBetweensFunc queryParser)
    where
        genBoolFuncParser = getGenBoolFuncParser queryParser

-- | Get a generic functions parser by providing its internal query parser.
getGenFuncParser :: QueryParser a -> FuncParser a
getGenFuncParser queryParser = FuncParser
    (parseAddFunc genFuncParser)
    (parseBitAndFunc genFuncParser)
    (parseBitOrFunc genFuncParser)
    (parseBitShiftLeftFunc genFuncParser)
    (parseBitShiftRightFunc genFuncParser)
    (parseDivideFunc genFuncParser)
    (parseModuloFunc genFuncParser)
    (parseMultiplyFunc genFuncParser)
    (parseSubstractFunc genFuncParser)
    (parseCountFunc queryParser)
     parseCurrentDateFunc
    (parseMaxFunc queryParser)
    (parseMinFunc queryParser)
     parseJokerFunc
     parseRandomFunc
    (parseSumFunc queryParser)
     parseCalcFoundRowsFunc
     parseFoundRowsFunc
    (parseInfixFunc queryParser)
    where
        genFuncParser = getGenFuncParser queryParser

-- | Get a generic join parser by providing its internal query parser.
getGenJoinParser :: QueryParser a -> JoinParser a
getGenJoinParser queryParser = JoinParser
    (parseJoinClauseFunc queryParser)
     parseJoinTColFunc
     parseJoinTTableFunc
    
-- | Get a parser using the provided statement parser.
getParser :: StmtParser a -> Parser a
getParser = Parser . parseStmtFunc

-- | Get a statement parser, using a given query parser.
getStmtParser :: QueryParser a -> StmtParser a
getStmtParser queryParser = StmtParser
    (parseDeleteFunc queryParser)
    (parseDropTableFunc queryParser)
    (parseDropViewFunc queryParser)
    (parseInsertFunc queryParser)
    (parseSelectFunc queryParser)
    (parseUpdateFunc queryParser)