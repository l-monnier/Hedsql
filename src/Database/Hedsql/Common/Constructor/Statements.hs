{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

{-|
Module      : Database/Hedsql/Common/Constructor/Statements.hs
Description : Constructor functions for statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructor functions for SQL statements which can then be parsed.
-}
module Database.Hedsql.Common.Constructor.Statements
    ( ToStmt
    , statement
    , statements
    ) where

import Database.Hedsql.Common.DataStructure

-- private functions.

class ToStmt a b where
    toStmt :: a c -> b c

instance ToStmt Table Statement where
    toStmt = CreateTableStmt

instance ToStmt CombinedQuery Statement where
    toStmt = CombinedQueryStmt

instance ToStmt CreateView Statement where
    toStmt = CreateViewStmt

instance ToStmt Delete Statement where
    toStmt = DeleteStmt

instance ToStmt DropTable Statement where
    toStmt = DropTableStmt

instance ToStmt DropView Statement where
    toStmt = DropViewStmt

instance ToStmt Insert Statement where
    toStmt = InsertStmt

instance ToStmt Select Statement where
    toStmt = SelectStmt
    
instance ToStmt Update Statement where
    toStmt = UpdateStmt

-- public functions.

-- | Create a statement.
statement :: ToStmt a Statement => a b -> Statement b
statement = toStmt

statements :: ToStmt a Statement => [a b] -> [Statement b]
statements = map statement