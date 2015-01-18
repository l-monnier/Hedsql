{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/DataStructure/Create.hs
Description : Constructor functions for columns.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

CREATE statement data type definitions.
-}

module Hedsql.Common.DataStructure.Create where

import Hedsql.Common.DataStructure.Select

import Control.Lens

-- | Timing of a constraint.
data ConstraintTiming a = ConstraintTiming
    { _constraintTimingType  :: ConstraintTimingType a
    , _constraintTimingCheck :: ConstraintTimingCheck a
    } deriving (Show)

-- | Type of a timing constraint.
data ConstraintTimingType a =
      Deferable
    | NotDeferable
      deriving (Show)

-- | Timing of a timing constraint.
data ConstraintTimingCheck a =
      InitiallyImmediate
    | InitiallyDeferred
      deriving (Show)

-- | CREATE TABLE statement.
data CreateTable a = CreateTable
    { _createTableIfNotExistParam :: Bool
    , _createTableTable           :: Table a
    , _createTableCols            :: [Column a]
    , _createTableConstraints     :: Maybe [TableConstraint a]
    } deriving (Show)

-- | CREATE VIEW query.
data CreateView a = CreateView
    { _viewName   :: [Char]
    , _viewSelect :: Select a
    } deriving (Show)

-- | Foreign key clause to be used in a table constraint of a CREATE statement.
data ForeignKeyClause a = ForeignKeyClause
    { _foreignKeyClauseTable  :: Table a
    , _foreignKeyClauseCols   :: [Column a]
    , _foreignKeyMatch        :: Maybe (Match a)
    , _foreignKeyClauseAction :: Maybe (OnAction a)
    } deriving (Show)

-- | Foreign key match type.
data Match a =
      Full
    | Partial
    | Simple
      deriving (Show)

-- | Table constraints to be used in CREATE statement.
data TableConstraint a = TableConstraint
    { _tableConstraintName       :: Maybe String
    , _tableConstraintConstraint :: TableConstraintType a
    , _tableConstraintTiming     :: Maybe (ConstraintTiming a)
    } deriving (Show)

-- | Table constraints types used in CREATE statement.
data TableConstraintType a =
      ForeignKey [Column a] (ForeignKeyClause a)
    | TableConstraintPrimaryKey [Column a]
    | TableConstraintUnique [Column a]
    | TableConstraintCheck (Condition a)
      deriving (Show)

-- Make the lenses.
makeLenses ''ConstraintTiming
makeLenses ''CreateTable
makeLenses ''CreateView
makeLenses ''ForeignKeyClause
makeLenses ''TableConstraint