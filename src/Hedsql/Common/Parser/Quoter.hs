{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Hedsql/Common/Parser/Quoter.hs
Description : Implementation of a default SQL quoter
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Provide elements which allow to quote correctly the different arguments of a
SQL statement.

It defines:
- an interface;
- a generic implementation;
- a generic quoting function which can be reused for other cases.
-}
module Hedsql.Common.Parser.Quoter
    ( Quoter(Quoter)
    , genQuoter
    , quoteElem
    , genQuote
    , quoteVal
    ) where

import Control.Lens

{-|
Interface defining the different quoting functions.
-}
data Quoter a = Quoter
    { _quoteElem :: String -> String -- ^ Quote a SQL element, such as a table
                                     --   or columns names.
    , _quoteVal  :: String -> String -- ^ Quote a raw value such as an integer.
    }
    
makeLenses ''Quoter

-- | Generic implementation of the quoter using " for elements and ' for values.
genQuoter :: Quoter a
genQuoter = Quoter
    (genQuote '"')
    (genQuote '\'')

{-|
Generic quotation function.

Quotes a text such as table to "table" (the quotation parameter
will vary depending on the provided parameter).

On top of this, it will escape quote inside the string by doubling those quotes.
For example: ta"ble will become "ta""ble".
-}
genQuote ::
       Char   -- ^ Quoting character (typically ", ' or `)
    -> String -- ^ String to quote.
    -> String -- ^ Returned quoted string.
genQuote quote text =
    concat
        [[quote]
        , concatMap (\x -> if x == quote then [quote, quote] else [x]) text
        ,[quote]
        ]