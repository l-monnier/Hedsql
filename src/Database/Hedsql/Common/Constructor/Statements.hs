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

import Database.Hedsql.Common.AST

-- private functions.

class ToStmt a b | a -> b where
    toStmt :: a -> b

instance ToStmt (Create a) (Statement a) where
    toStmt = CreateStmt

instance ToStmt (Delete a) (Statement a) where
    toStmt = DeleteStmt

instance ToStmt (Drop a) (Statement a) where
    toStmt = DropStmt

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

-- | Create many statements from a list.
statements :: ToStmt a (Statement b) => [a] -> [Statement b]
statements = map statement