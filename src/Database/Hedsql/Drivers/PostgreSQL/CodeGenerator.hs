{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database/Hedsql/Drivers/PostgreSQL/CodeGenerator.hs
Description : PostgreSQL code generator implementation.
Copyright   : (c) Leonard Monnier2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL code generator implementation.
-}
module Database.Hedsql.Drivers.PostgreSQL.CodeGenerator
    ( T.CodeGenerator
    , codeGen
    , codeGenP
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.CodeGenerator
import Database.Hedsql.Drivers.PostgreSQL.Driver
import qualified Database.Hedsql.Common.CodeGenerator.Type as T

import Control.Lens
import Database.Hedsql.Common.PrettyPrint

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

{-|
Return True if one of the provided constraint is a PRIMARY KEY.
with auto increment.
-}
hasAutoIncrement :: [ColConstraint a] -> Bool
hasAutoIncrement =
    all (\x -> isAIPK $ x^.colConstraintType)
    where
        isAIPK (Primary isAI) = isAI
        isAIPK _              = False

-- | Create the PostgreSQL code generator.
postgreSQLCodeGenerator :: CodeGenerator PostgreSQL
postgreSQLCodeGenerator =
    getCodeGenerator postgreSQLCodeGenerator
        { _codeGenColConstType =
              codeGenPostgreSQLColConstTypeFunc postgreSQLCodeGenerator
        , _codeGenColCreate =
              codeGenPostgreSqlColCreateFunc    postgreSQLCodeGenerator
        }

{-|
The AUTOINCREMENT constraint is not a constraint in PostgreSQL.
Instead, the "serial" data type is used.

We must therefore remove the AUTOINCREMENT constraint when parsing
a PRIMARY KEY column constraint.
-}
codeGenPostgreSQLColConstTypeFunc ::
       CodeGenerator a
    -> ColConstraintType a
    -> Doc
codeGenPostgreSQLColConstTypeFunc codeGenerator c =
    case c of
        (Primary _) -> "PRIMARY KEY"
        _           -> codeGenColConstTypeFunc codeGenerator c

{- |
    Custom function for PostgreSQL for the creation of a table.
    The difference with the default implementation is that a PRIMARY KEY of
    type Integer with an AUTOINCREMENT constraints get translated as a "serial".
-}
codeGenPostgreSqlColCreateFunc :: CodeGenerator a -> Int -> ColWrap a -> Doc
codeGenPostgreSqlColCreateFunc codeGenerator _ (ColWrap c) =
        codeGenCols (DataTypeWrap $ c^.colType) (c^.colConstraints)
    where
        codeGenCols (DataTypeWrap Integer) colConsts@(_:_) =
            if hasAutoIncrement colConsts
            then cName <+> "serial" <+> consts colConsts
            else cName <+> "integer" <+> consts colConsts
        codeGenCols cType colConsts = hsep
            [ cName
            , _codeGenDataType codeGenerator cType
            , consts colConsts
            ]

        cName = _quoteElem codeGenerator $ c^.colName

        consts [] = empty
        consts cs = csep $ map (_codeGenColConst codeGenerator) cs

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
codeGen :: T.CodeGenerator PostgreSQL
codeGen = renderRaw . _codeGenStmt postgreSQLCodeGenerator . statement

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string in pretty print mode.
-}
codeGenP :: T.CodeGenerator PostgreSQL
codeGenP = renderP . _codeGenStmt postgreSQLCodeGenerator . statement
