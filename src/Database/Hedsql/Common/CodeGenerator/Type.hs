{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

{-|
Module      : Database/Hedsql/Common/CodeGenerator/Type.hs
Description : Type alias for code generation functions.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Type alias for SQL code generation functions.
-}
module Database.Hedsql.Common.CodeGenerator.Type
    ( CodeGenerator
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor
import Data.Text.Lazy (Text)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Type alias for a generic code generator function.

This allow to simplify the type signature of the code generator functions but
mainly it allows to use code generation functions as type class parameter to
provide custom function implementation depending on the used code generator.
For example:

@
class Stmt codeGenerator where
    createStmt :: CodeGenerator codeGenerator -> Statement codeGenerator

instance Stmt PostgreSQL where
    createStmt codeGenerator = ...

instance Stmt MariaDb where
    createStmt codeGenerator = ...
@
-}
type CodeGenerator codeGenerator =
    forall a. ToStmt a (Statement codeGenerator) => a -> Text
