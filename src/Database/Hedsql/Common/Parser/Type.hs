{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

{-|
Module      : Database/Hedsql/Common/Parser/Type.hs
Description : Type alias for parser functions.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Type alias for parser functions.
-}
module Database.Hedsql.Common.Parser.Type
    ( Parser
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST
import Database.Hedsql.Common.Constructor

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Type alias for a generic parser function.

This allow to simplify the type signature of the parser functions but mainly it
allow to use parser functions as type class parameter to provide custom function
implementation depending on the used parser. For example:

@
class Stmt parser where
    createStmt :: Parser parser -> Statement parser

instance Stmt PostgreSQL where
    createStmt parser = ...

instance Stmt MariaDb where
    createStmt parser = ...
@
-}
type Parser parser = forall a. ToStmt a (Statement parser) => a -> String
