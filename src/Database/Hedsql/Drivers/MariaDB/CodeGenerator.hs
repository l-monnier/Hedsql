{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database/Hedsql/Drivers/MariaDB/CodeGenerator.hs
Description : MariaDB code generator.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

MariaDB code generator implementation.
-}
module Database.Hedsql.Drivers.MariaDB.CodeGenerator
    ( T.CodeGenerator
    , codeGen
    , codeGenP
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.Constructor
import Database.Hedsql.Common.AST
import Database.Hedsql.Common.CodeGenerator
import Database.Hedsql.Drivers.MariaDB.Driver
import qualified Database.Hedsql.Common.CodeGenerator.Type as T

import Data.Char
import Data.Text.Lazy(pack)
import Database.Hedsql.Common.PrettyPrint

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Generate Code For MariaDB data types.
mariaDBDataTypeFunc :: DataTypeWrap MariaDB -> Doc
mariaDBDataTypeFunc = text . pack . map toUpper . show . codeGenDataTypeFunc

mariaDBExprFunc :: ExprWrap MariaDB -> Doc
mariaDBExprFunc (ExprWrap LastInsertId) = "LAST_INSERT_ID()"
mariaDBExprFunc e = codeGenExprFunc mariaDBCodeGenerator e

-- | Create the MariaDB code generator.
mariaDBCodeGenerator :: CodeGenerator MariaDB
mariaDBCodeGenerator = getCodeGenerator mariaDBCodeGenerator {
      _codeGenDataType = mariaDBDataTypeFunc
    , _codeGenExpr     = mariaDBExprFunc
    }

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string.
-}
codeGen :: T.CodeGenerator MariaDB
codeGen = renderRaw . _codeGenStmt mariaDBCodeGenerator . statement

{-|
Convert a SQL statement (or something which can be coerced to a statement)
to a SQL string in pretty print mode.
-}
codeGenP :: T.CodeGenerator MariaDB
codeGenP = renderP . _codeGenStmt mariaDBCodeGenerator . statement
