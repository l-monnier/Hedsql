{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Database/Hedsql/Drivers/SqLite/CodeGenerator.hs
Description : SqLite code generator.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SqLite code generator implementation.
-}
module Database.Hedsql.Drivers.SqLite.CodeGenerator
    ( T.CodeGenerator
    , codeGen
    , codeGenP
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.Common.AST
import           Database.Hedsql.Common.Constructor
import           Database.Hedsql.Common.CodeGenerator
import           Database.Hedsql.Drivers.SqLite.Driver
import qualified Database.Hedsql.Common.CodeGenerator.Type as T

import           Data.Text.Lazy                        (toUpper)

import           Database.Hedsql.Common.PrettyPrint

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Generate code for SqLite data types.
sqLiteDataTypeFunc :: DataTypeWrap SqLite -> Doc
sqLiteDataTypeFunc dataType =
    case dataType of
        DataTypeWrap (Char lenght) ->
            "CHARACTER" <> parens (int lenght)
        _ ->
            text $ toUpper $ renderRaw $ codeGenDataTypeFunc dataType

{-|
Generate code for a SqLite value.
Booleans in SqLite are represented by numeric values only (0 or 1).
-}
sqLiteCodeGenValueFunc :: ValueWrap SqLite -> Doc
sqLiteCodeGenValueFunc (ValueWrap (BoolVal True))  = "1"
sqLiteCodeGenValueFunc (ValueWrap (BoolVal False)) = "0"
sqLiteCodeGenValueFunc val = codeGenValueFunc sqLiteCodeGenerator val

-- | Create the SqLite function code generator.
sqLiteExprFunc :: ExprWrap SqLite -> Doc
sqLiteExprFunc (ExprWrap CurrentDate) = "Date('now')"
sqLiteExprFunc (ExprWrap LastInsertId) = "last_insert_rowid()"
sqLiteExprFunc e = codeGenExprFunc sqLiteCodeGenerator e

-- | Create the SqLite code generator.
sqLiteCodeGenerator :: CodeGenerator SqLite
sqLiteCodeGenerator = CodeGenerator
    (codeGenStmtFunc sqLiteCodeGenerator)
    sqLiteExprFunc
    (codeGenTableConstFunc sqLiteCodeGenerator)
    (codeGenTableConstTypeFunc sqLiteCodeGenerator)
    (codeGenFkFunc sqLiteCodeGenerator)
    codeGenMatchFunc
    (codeGenOnActionFunc sqLiteCodeGenerator)
    codeGenActionFunc
    codeGenConstTimingFunc
    (codeGenViewFunc sqLiteCodeGenerator)
    (codeGenColCreateFunc sqLiteCodeGenerator)
    sqLiteDataTypeFunc
    (codeGenColConstFunc sqLiteCodeGenerator)
    (codeGenColConstTypeFunc sqLiteCodeGenerator)
    (codeGenCreateFunc sqLiteCodeGenerator)
    (codeGenDropFunc sqLiteCodeGenerator)
    (codeGenTableNameFunc sqLiteCodeGenerator)
    (codeGenTableRefFunc sqLiteCodeGenerator)
    (codeGenTableRefAsFunc sqLiteCodeGenerator)
    (codeGenColFunc sqLiteCodeGenerator)
    (codeGenColDefFunc sqLiteCodeGenerator)
    (codeGenColRefDefFunc sqLiteCodeGenerator)
    (codeGenColRefFunc sqLiteCodeGenerator)
    sqLiteCodeGenValueFunc
    (codeGenSelectFunc sqLiteCodeGenerator)
    codeGenCombinationFunc
    (codeGenSelectTypeFunc sqLiteCodeGenerator)
    (codeGenSelectionFunc sqLiteCodeGenerator)
    (codeGenFromFunc sqLiteCodeGenerator)
    (codeGenJoinFunc sqLiteCodeGenerator)
    (codeGenJoinClauseFunc sqLiteCodeGenerator)
    codeGenJoinTColFunc
    codeGenJoinTTableFunc
    (codeGenWhereFunc sqLiteCodeGenerator)
    (codeGenGroupByFunc sqLiteCodeGenerator)
    (codeGenHavingFunc sqLiteCodeGenerator)
    (codeGenOrderByFunc sqLiteCodeGenerator)
    (codeGenSortRefFunc sqLiteCodeGenerator)
    codeGenSortOrderFunc
    codeGenSortNullFunc
    (codeGenAssgnmtFunc sqLiteCodeGenerator)
    (codeGenInsertAssignFunc sqLiteCodeGenerator)
    (codeGenDeleteFunc sqLiteCodeGenerator)
    (codeGenInsertFunc sqLiteCodeGenerator)
    (codeGenUpdateFunc sqLiteCodeGenerator)
    (codeGenReturningFunc sqLiteCodeGenerator)
    quoteElemFunc
    quoteValFunc

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
codeGen :: T.CodeGenerator SqLite
codeGen = renderRaw ._codeGenStmt sqLiteCodeGenerator . statement

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string in pretty print mode.
-}
codeGenP :: T.CodeGenerator SqLite
codeGenP = renderP ._codeGenStmt sqLiteCodeGenerator . statement
