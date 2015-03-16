{-|
Module      : Database/Hedsql/Common/Parser.hs
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
module Database.Hedsql.Common.Parser
    ( module Database.Hedsql.Common.Parser.Interface
    , module Database.Hedsql.Common.Parser.Queries
    , genQueryParser
    , genTableParser
    , getGenJoinParser
    , getParser
    , getQueryParser
    , getStmtParser
    , getTableParser
    ) where

import Database.Hedsql.Common.Parser.Interface
import Database.Hedsql.Common.Parser.Queries
import Database.Hedsql.Common.Parser.TableManipulations
import qualified Database.Hedsql.Common.Parser.Quoter as Q

import Control.Lens

-- Public.

-- | Generic query parser.
genQueryParser :: QueryParser a
genQueryParser =
    getQueryParser
        genQueryParser
        genTableParser

-- | Generic table parser.
genTableParser :: TableParser a
genTableParser = getTableParser genQueryParser genTableParser

{-|
Return a query parser using the provided query parser.
-}
getQueryParser ::
       QueryParser a
    -> TableParser a
    -> QueryParser a
getQueryParser queryParser tableParser = QueryParser
    (parseAssgnmtFunc queryParser)
    (parseColFunc queryParser)
    (parseColRefFunc queryParser)
    (parseColRefDefFunc queryParser)
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
        genJoinParser     = getGenJoinParser queryParser
        stmtParser        = getStmtParser queryParser tableParser

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
getStmtParser :: QueryParser a -> TableParser a -> StmtParser a
getStmtParser queryParser tableParser = StmtParser
    (parseCombinedFunc $ getStmtParser queryParser tableParser)
    (parseCreateTableFunc tableParser)
    (parseCreateViewFunc tableParser)
    (parseDeleteFunc queryParser)
    (parseDropTableFunc queryParser)
    (parseDropViewFunc queryParser)
    (parseInsertFunc queryParser)
    (parseSelectFunc queryParser)
    (parseStmtFunc $ getStmtParser queryParser tableParser)
    (parseUpdateFunc queryParser)
    
-- | Return a table parser using the provided query and table parsers.
getTableParser ::
       QueryParser a
    -> TableParser a
    -> TableParser a
getTableParser queryParser tableParser = TableParser
    parseActionFunc
    (parseColFunc queryParser)
    (parseColCreateFunc tableParser)
    (parseColConstFunc tableParser)
    (parseColConstTypeFunc tableParser)
    (parseConstTimingFunc tableParser)
    parseConstTimingCheckFunc
    parseConstTimingTypeFunc
    (parseCreateTableFunc tableParser)
    (parseCreateViewFunc tableParser)
    parseDataTypeFunc
    (parseExprFunc queryParser stmtParser)
    (parseFkClauseFunc tableParser)
    parseMatchClauseFunc
    (parseOnActionFunc tableParser)
    (parseTableConstFunc tableParser)
    (parseTableConstTypeFunc tableParser)
    (parseSelectFunc queryParser)
    (parseTableNameFunc queryParser)
    (Q.genQuoter ^. Q.quoteElem)
    where
        stmtParser = getStmtParser queryParser tableParser