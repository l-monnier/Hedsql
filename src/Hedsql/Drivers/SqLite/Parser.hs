{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Hedsql/Drivers/SqLite/Parser.hs
Description : SqLite parser.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SqLite parser implementation.
-}
module Hedsql.Drivers.SqLite.Parser
    ( parse
    ) where

import Hedsql.Common.Constructor.Statements
import Hedsql.Common.DataStructure
import Hedsql.Common.Parser
import Hedsql.Drivers.SqLite.Driver

import Control.Lens

-- Private.

-- | Create the SqLite function parser.
sqLiteFuncFunc :: Function SqLite -> String
sqLiteFuncFunc CurrentDate = "Date('now')"
sqLiteFuncFunc func        = parseFuncFunc sqLiteQueryParser func
    

-- | Create the SqLite parser.
sqLiteParser :: Parser SqLite
sqLiteParser = getParser $ getStmtParser sqLiteQueryParser genTableParser
    
-- | Create the SqLite query parser.
sqLiteQueryParser :: QueryParser SqLite
sqLiteQueryParser =
    (getQueryParser sqLiteQueryParser genTableParser)
        & parseFunc .~ sqLiteFuncFunc

-- Public.

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
parse :: CoerceToStmt a Statement => (a SqLite) -> String
parse = (sqLiteParser^.parseStmt).statement