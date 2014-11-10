{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/Parser/Interface.hs
Description : SQL parser interface.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Define the high level functionalities of a SQL Parser.
The different elements of the parser can be accessed via lenses.

The Parser definition is very generic as more specialized parts can later
be defined for each component thanks to currying.

For example, for parseSelect we could define a function with the following
type signature:

> parseSelectFunc :: QueryParser a -> Select a -> String

Then, when constructing a parser, we can provide to that function 
a dedicated QueryParser. This parser can then have specialized functions
for each element of a SELECT statement such as:

> data QueryParser a = QueryParser
>    {
>      _parseFrom  :: From a  -> String
>    , _parseWhere :: Where a -> String
>    , etc.
>    }
-}
module Hedsql.Common.Parser.Interface
    (
       Parser(Parser)
    ,  parseDelete
    ,  parseDropTable
    ,  parseDropView
    ,  parseSelect
    ) where

import Control.Lens
import Hedsql.Common.DataStructure.Base

{-|
Interface which defines the top level functions of a SQL Parser.
-}
data Parser a = Parser
    {
      _parseDelete    :: Delete a    -> String
    , _parseDropTable :: DropTable a -> String
    , _parseDropView  :: DropView a  -> String
    , _parseSelect    :: Select a    -> String
    }
    
makeLenses ''Parser