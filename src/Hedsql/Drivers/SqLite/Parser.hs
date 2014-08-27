-- file : Hedsql/Drivers/SqLite/Parser.hs


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    SQLite parser implementation.
-}

module Hedsql.Drivers.SqLite.Parser (
      module Hedsql.Common.DefaultParser
    , toSqlString
    ) where

import Hedsql.Common.DataStructure.Base
import Hedsql.Common.Driver 
import Hedsql.Common.DefaultParser
import Hedsql.Common.Parser
import Hedsql.Drivers.SqLite.Driver

{-|
    Parser instance for the SqLite driver. It is basically a call to the
    "toSqlLiteString" function of the "SqLiteParser" type class.
-}
instance (DefaultParser SqLite b, SqLiteParser b) => Parser SqLite b where
    toSqlString _ b = toSqLiteString b
 
-- | SQLite parser class.
class SqLiteParser a where
    toSqLiteString :: (DefaultParser SqLite a) => a -> String

{-|
    Generic default instance used if no specific instance exists for the type.
    This is basically a call to the "toDefaultSqlString" function of the
    "DefaultParser" type class.
-}
instance SqLiteParser a where
    toSqLiteString = toDefaultSqlString SqLite

-- | The CURRENT_DATE function is written "Date('now')" in SqLite.
instance SqLiteParser CurrentDate where
    toSqLiteString  _ = "Date('now')"