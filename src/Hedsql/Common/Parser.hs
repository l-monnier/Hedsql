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
    , getGenParser
    ) where

import Hedsql.Common.Parser.BoolFunctions
import Hedsql.Common.Parser.Functions
import Hedsql.Common.Parser.Interface
import Hedsql.Common.Parser.Queries
import qualified Hedsql.Common.Parser.Quoter as Q

import Control.Lens

-- | Generic query parser.
genQueryParser :: QueryParser a
genQueryParser = QueryParser
    (parseColFunc genQueryParser)
    (parseColRefFunc genQueryParser)
    (parseColRefDefFunc genQueryParser)
    (parseConditionFunc genQueryParser)
    (parseFuncFunc genFuncParser)
    (parseFuncBoolFunc genBoolFuncParser)
    (parseExprFunc genQueryParser genParser)
    (parseFromFunc genQueryParser)
    (parseJoinFunc genQueryParser genJoinParser)
    (parseGroupByFunc genQueryParser)
    (parseOrderByFunc genQueryParser)
     parseSortNullFunc
    (parseSortRefFunc genQueryParser)
     parseSortOrderFunc
    (parseTableNameFunc genQueryParser)
    (parseTableRefFunc genParser genQueryParser)
    (parseTableRefAsFunc genQueryParser)
    (parseValueFunc genQueryParser)
    (parseWhereFunc genQueryParser)
    (Q.genQuoter ^. Q.quoteElem)
    (Q.genQuoter ^. Q.quoteVal)
    where
        genBoolFuncParser = getGenBoolFuncParser genQueryParser
        genFuncParser = getGenFuncParser genQueryParser
        genJoinParser = getGenJoinParser genQueryParser
        genParser = getGenParser genQueryParser

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
    (parseInfixFunc queryParser)
    where
        genFuncParser = getGenFuncParser queryParser

-- | Get a generic join parser by providing its internal query parser.
getGenJoinParser :: QueryParser a -> JoinParser a
getGenJoinParser queryParser = JoinParser
    (parseJoinClauseFunc queryParser)
     parseJoinTColFunc
     parseJoinTTableFunc

-- | Get a generic parser by providing its internal parsers.
getGenParser :: QueryParser a -> Parser a
getGenParser queryParser = Parser
    (parseDeleteFunc queryParser)
    (parseDropTableFunc queryParser)
    (parseDropViewFunc queryParser)
    (parseSelectFunc queryParser)