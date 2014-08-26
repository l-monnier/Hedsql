-- file : Hedsql/Common/DataStructure/Create
{-# LANGUAGE TemplateHaskell #-}

{-|
    CREATE statement data type definitions.
-}

module Hedsql.Common.DataStructure.Create where

import Hedsql.Common.DataStructure.Select
import Control.Lens

-- | CREATE TABLE statement.
data CreateTable = CreateTable {
      _createTableIfNotExistParam :: Bool
    , _createTableTable :: Table
    , _createTableCols :: [Column]
    , _createTableConstraints :: Maybe [TableConstraint]
} deriving (Show)

-- | CREATE VIEW query.
data CreateView = CreateView {
      _viewName :: [Char]
    , _viewSelect :: SelectQuery
}

-- | Table constraints to be used in CREATE statement.
data TableConstraint = TableConstraint {
      _tableConstraintName :: Maybe String
    , _tableConstraintConstraint :: TableConstraintType
    , _tableConstraintTiming :: Maybe ConstraintTiming
} deriving (Show)

-- | Table constraints types used in CREATE statement.
data TableConstraintType =
      ForeignKey [Column] ForeignKeyClause
    | TableConstraintPrimaryKey [Column]
    | TableConstraintUnique [Column]
    | TableConstraintCheck Condition
      deriving (Show)

-- | Foreign key clause to be used in a table constraint of a CREATE statement.
data ForeignKeyClause = ForeignKeyClause {
      _foreignKeyClauseTable :: Table
    , _foreignKeyClauseCols :: [Column]
    , _foreignKeyMatch :: Maybe Match
    , _foreignKeyClauseAction :: Maybe OnAction
} deriving (Show)

-- | Foreign key match type.
data Match =
      Full
    | Partial
    | Simple
      deriving (Show)

-- | Timing of a constraint.
data ConstraintTiming = ConstraintTiming {
       _constraintTimingType :: ConstraintTimingType
     , _constraintTimingCheck :: ConstraintTimingCheck
} deriving (Show)

-- | Type of a timing constraint.
data ConstraintTimingType = Deferable | NotDeferable deriving (Show)

-- | Timing of a timing constraint.
data ConstraintTimingCheck = InitiallyImmediate | InitiallyDeferred deriving (Show)

-- Make the lenses.
makeLenses ''ConstraintTiming
makeLenses ''CreateTable
makeLenses ''CreateView
makeLenses ''ForeignKeyClause
makeLenses ''TableConstraint