{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-} 

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

class ToStmt a b | a -> b where
    toStmt :: a -> b

instance ToStmt (Table a) (Statement a) where
    toStmt = CreateTableStmt

instance ToStmt (CombinedQuery a) (Statement a) where
    toStmt = CombinedQueryStmt

instance ToStmt (CreateView a) (Statement a) where
    toStmt = CreateViewStmt

instance ToStmt (Delete a) (Statement a) where
    toStmt = DeleteStmt

instance ToStmt (DropTable a) (Statement a) where
    toStmt = DropTableStmt

instance ToStmt (DropView a) (Statement a) where
    toStmt = DropViewStmt

instance ToStmt (Insert a) (Statement a) where
    toStmt = InsertStmt

instance ToStmt (Select b a) (Statement a) where
    toStmt = SelectStmt . SelectWrap

instance ToStmt (SelectWrap a) (Statement a) where
    toStmt = SelectStmt
    
instance ToStmt (Update a) (Statement a) where
    toStmt = UpdateStmt

-- public functions.

-- | Create a statement.
statement :: ToStmt a (Statement b) => a -> Statement b
statement = toStmt

statements :: ToStmt a (Statement b) => [a] -> [Statement b]
statements = map statement