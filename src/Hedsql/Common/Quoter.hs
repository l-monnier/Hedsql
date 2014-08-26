-- file : Hedsql/Common/Quoter.hs

{-# LANGUAGE FlexibleInstances #-}

{-|
    Quoter class definition and default quoter.
-}

module Hedsql.Common.Quoter where

import qualified Data.Text as Text

{-|
    Quote a SQL identifiers such as the name of a table.
    All strings are converted to UTF-16 (see Data.Text library for more details).
    Quoting characters such as " are escaped.
-}
class Quoter a where
    quoteSql :: a -> String -> String

instance Quoter a where
    -- | The default quoter uses a double quote " to enclose table and column names.
    quoteSql a text =
        "\"" ++  Text.unpack (Text.replace (Text.pack "\"") (Text.pack "\"\"") (Text.pack text)) ++ "\""