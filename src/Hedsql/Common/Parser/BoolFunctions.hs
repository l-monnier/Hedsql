{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/Parser/BoolFunctions.hs
Description : Implementation of the SQL boolean functions parsers.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL boolean functions parsers - functions returning
boolean value.
-}
module Hedsql.Common.Parser.BoolFunctions
    ( -- Interface.
      FuncBoolParser(FuncBoolParser)
    , parseBetween
    , parseEqual
    , parseExists
    , parseGreaterThan
    , parseGreaterThanOrEqTo
    , parseIn
    , parseIsDistinctFrom
    , parseIsFalse
    , parseIsNotDistinctFrom
    , parseIsNotFalse
    , parseIsNotNull
    , parseIsNotTrue
    , parseIsNotUnknown
    , parseIsNull
    , parseIsTrue
    , parseIsUnknown
    , parseLike
    , parseNotBetween
    , parseNotEqual
    , parseNotIn
    , parseSmallerThan
    , parseSmallerThanOrEqTo
    , parseInfix
    , parseIs
    , parseBetweens
    
      -- Dispatch function implementation.
    , parseFuncBoolFunc
    
      -- Generic functions implementation.
    , parseBetweenFunc
    , parseEqualFunc
    , parseExistsFunc
    , parseGreaterThanFunc
    , parseGreaterThanOrEqToFunc
    , parseInFunc
    , parseIsDistinctFromFunc
    , parseIsFalseFunc
    , parseIsNotDistinctFromFunc
    , parseIsNotFalseFunc
    , parseIsNotNullFunc
    , parseIsNotTrueFunc
    , parseIsNotUnknownFunc
    , parseIsNullFunc
    , parseIsTrueFunc
    , parseIsUnknownFunc
    , parseLikeFunc
    , parseNotBetweenFunc
    , parseNotEqualFunc
    , parseNotInFunc
    , parseSmallerThanFunc
    , parseSmallerThanOrEqToFunc
    , parseInfixFunc
    , parseIsFunc
    , parseBetweensFunc
    ) where

import Hedsql.Common.DataStructure
import Hedsql.Common.Parser.Queries

import Control.Lens

-- Definition of the parsers interfaces.

data FuncBoolParser a = FuncBoolParser
    { _parseBetween           :: Between a           -> String
    , _parseEqual             :: Equal a             -> String
    , _parseExists            :: Exists a            -> String
    , _parseGreaterThan       :: GreaterThan a       -> String
    , _parseGreaterThanOrEqTo :: GreaterThanOrEqTo a -> String
    , _parseIn                :: In a                -> String
    , _parseIsDistinctFrom    :: IsDistinctFrom a    -> String
    , _parseIsFalse           :: IsFalse a           -> String
    , _parseIsNotDistinctFrom :: IsNotDistinctFrom a -> String
    , _parseIsNotFalse        :: IsNotFalse a        -> String
    , _parseIsNotNull         :: IsNotNull a         -> String
    , _parseIsNotTrue         :: IsNotTrue a         -> String
    , _parseIsNotUnknown      :: IsNotUnknown a      -> String
    , _parseIsNull            :: IsNull a            -> String
    , _parseIsTrue            :: IsTrue a            -> String
    , _parseIsUnknown         :: IsUnknown a         -> String
    , _parseLike              :: Like a              -> String
    , _parseNotBetween        :: NotBetween a        -> String
    , _parseNotEqual          :: NotEqual a          -> String
    , _parseNotIn             :: NotIn a             -> String
    , _parseSmallerThan       :: SmallerThan a       -> String
    , _parseSmallerThanOrEqTo :: SmallerThanOrEqTo a -> String
    
    -- Helper functions
    , _parseInfix    :: String -> ColRef a -> ColRef a -> String
    , _parseIs       :: ColRef a -> String -> String
    , _parseBetweens :: Bool -> ColRef a -> ColRef a -> ColRef a -> String
    }

makeLenses ''FuncBoolParser

-- Helpers functions of the function parser.

-- | Parse a "BETWEEN" or "NOT BETWEEN" statement.
parseBetweensFunc ::
       QueryParser a
    -> Bool -- ^ If true, it will be a "BETWEEN" function.
            --   Otherwise, it will be a "NOT BETWEEN" one.
    -> ColRef a
    -> ColRef a
    -> ColRef a
    -> String


-- | Parse an infix function.
parseInfixFunc ::
       QueryParser a
    -> String   -- ^ Name of the function ("=", ">", etc.).
    -> ColRef a -- ^ Left parameter.
    -> ColRef a -- ^ Right parameter.
    -> String


-- | Parse a "IS ..." function. Such as "IS TRUE".
parseIsFunc ::
       QueryParser a
    -> ColRef a
    -> String -- ^ Name of the function, such as "TRUE".
    -> String
parseIsFunc parser colRef text =
    concat [parser^.parseColRef $ colRef, "IS ", text]

-- Generic implementation of the functions.

-- | Parse a function returning a boolean value.
parseFuncBoolFunc :: FuncBoolParser a -> FuncBool a -> String
parseFuncBoolFunc parser funcBool =
    case funcBool of
        Between    ref lower higher -> parseBetweens True ref lower higher
        Equal             ref1 ref2 -> parseInfix "=" ref1 ref2
        Exists            colRef    -> concat
                                            ["(EXISTS "
                                            , parser^.parseColRef $ colRef
                                            , ")"
                                            ]
        GreaterThan       ref1 ref2 -> parseInfix ">" ref1 ref2
        GreaterThanOrEqTo ref1 ref2 -> parseInfix ">=" ref1 ref2
        In                ref1 ref2 -> parseInfix "IN" ref1 ref2
        IsDistinctFrom    ref1 ref2 -> parseInfix "IS DISTINCT FROM" ref1 ref2
        IsFalse           expr      -> parseIs expr "FALSE"
        IsNotDistinctFrom ref1 ref2 -> parseInfix
                                            "IS NOT DISTINCT FROM"
                                            ref1
                                            ref2
        IsNotFalse        expr      -> parseIs expr "NOT FALSE"
        IsNotNull         expr      -> parseIs expr "NOT NULL"
        IsNotTrue         expr      -> parseIs expr "NOT TRUE"
        IsNotUnknown      expr      -> parseIs expr "NOT UNKNOWN"
        IsNull            expr      -> parseIs expr "NULL"
        IsTrue            expr      -> parseIs expr "TRUE"
        IsUnknown         expr      -> parseIs expr "UNKNOWN"
        Like              ref1 ref2 -> parseInfix "LIKE" ref1 ref2
        NotBetween ref lower higher -> parseBetweens FALSE ref lower higher
        NotEqual          ref1 ref2 -> parseInfix "<>" ref1 ref2
        NotIn             ref1 ref2 -> parseInfix "NOT IN" ref1 ref2
        SmallerThan       ref1 ref2 -> parseInfix "<" ref1 ref2
        SmallerThanOrEqTo ref1 ref2 -> parseInfix "<=" ref1 ref2
    where
        parseBetweens func colRef lower higher =
            concat
                [ "("
                , parser^.parseColRef $ colRef
                , " "
                , if func then "" else "NOT"
                , " BETWEEN "
                , parser^.parseColRef $ lower
                , " AND "
                , parser^.parseColRef $ higher
                , ")"
                ]
        parseInfix name colRef1 colRef2 =
            concat
                [ "("
                , parser^.parseColRef $ colRef1
                , " "
                , name
                , " "
                , parser^.parseColRef $ colRef2
                , ")"
                ]
        parseIs colRef text =
            concat
                [parser^.parseColRef $ colRef
                , "IS "
                , text
                ]