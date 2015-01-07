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
    (
    -- Interface.
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
    {
      _parseBetween           :: Between a           -> String
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
parseBetweensFunc parser func colRef lower higher = concat
    [
      "("
    , parser^.parseColRef $ colRef
    , " "
    , if func then "" else "NOT"
    , " BETWEEN "
    , parser^.parseColRef $ lower
    , " AND "
    , parser^.parseColRef $ higher
    , ")"
    ]

-- | Parse an infix function.
parseInfixFunc ::
       QueryParser a
    -> String   -- ^ Name of the function ("=", ">", etc.).
    -> ColRef a -- ^ Left parameter.
    -> ColRef a -- ^ Right parameter.
    -> String
parseInfixFunc parser name colRef1 colRef2 = concat
    [
      "("
    , parser^.parseColRef $ colRef1
    , " "
    , name
    , " "
    , parser^.parseColRef $ colRef2
    , ")"
    ]

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
        BetweenF           func -> parser^.parseBetween $ func
        EqualF             func -> parser^.parseEqual $ func
        ExistsF            func -> parser^.parseExists $ func
        GreaterThanF       func -> parser^.parseGreaterThan $ func
        GreaterThanOrEqToF func -> parser^.parseGreaterThanOrEqTo $ func
        InF                func -> parser^.parseIn $ func
        IsDistinctFromF    func -> parser^.parseIsDistinctFrom $ func
        IsFalseF           func -> parser^.parseIsFalse $ func
        IsNotDistinctFromF func -> parser^.parseIsNotDistinctFrom $ func
        IsNotFalseF        func -> parser^.parseIsNotFalse $ func
        IsNotNullF         func -> parser^.parseIsNotNull $ func
        IsNotTrueF         func -> parser^.parseIsNotTrue $ func
        IsNotUnknownF      func -> parser^.parseIsNotUnknown $ func
        IsNullF            func -> parser^.parseIsNull $ func
        IsTrueF            func -> parser^.parseIsTrue $ func
        IsUnknownF         func -> parser^.parseIsUnknown $ func
        LikeF              func -> parser^.parseLike $ func
        NotBetweenF        func -> parser^.parseNotBetween $ func
        NotEqualF          func -> parser^.parseNotEqual $ func
        NotInF             func -> parser^.parseNotIn $ func
        SmallerThanF       func -> parser^.parseSmallerThan $ func
        SmallerThanOrEqToF func -> parser^.parseSmallerThanOrEqTo $ func

-- | Parse "BETWEEN".
parseBetweenFunc :: FuncBoolParser a -> Between a -> String
parseBetweenFunc parser (Between ref lower higher) =
    (parser^.parseBetweens) True ref lower higher

-- | Parse "=".
parseEqualFunc :: FuncBoolParser a -> Equal a -> String
parseEqualFunc parser (Equal ref1 ref2) = (parser^.parseInfix) "=" ref1 ref2

-- | Parse "EXISTS".
parseExistsFunc :: QueryParser a -> Exists a -> [Char]
parseExistsFunc parser (Exists colRef) =
    concat ["(EXISTS ", parser^.parseColRef $ colRef, ")"]

-- | Parse ">".
parseGreaterThanFunc :: FuncBoolParser a -> GreaterThan a -> String
parseGreaterThanFunc parser (GreaterThan ref1 ref2) =
    (parser^.parseInfix) ">" ref1 ref2

-- | Parse ">=".
parseGreaterThanOrEqToFunc :: FuncBoolParser a -> GreaterThanOrEqTo a -> String
parseGreaterThanOrEqToFunc parser (GreaterThanOrEqTo ref1 ref2) =
    (parser^.parseInfix) ">=" ref1 ref2

-- | Parse "IN".
parseInFunc :: FuncBoolParser a -> In a -> String
parseInFunc parser (In ref1 ref2) = (parser^.parseInfix) "IN" ref1 ref2

-- | Parse "IS DISTINCT FROM".
parseIsDistinctFromFunc :: FuncBoolParser a -> IsDistinctFrom a -> String
parseIsDistinctFromFunc parser (IsDistinctFrom ref1 ref2) =
    (parser^.parseInfix) "IS DISTINCT FROM" ref1 ref2

-- | Parse "IS FALSE".
parseIsFalseFunc :: FuncBoolParser a -> IsFalse a -> String
parseIsFalseFunc parser (IsFalse expr) = (parser^.parseIs) expr "FALSE"

-- | Parse "IS NOT DISTINCT FROM".
parseIsNotDistinctFromFunc :: FuncBoolParser a -> IsNotDistinctFrom a -> String
parseIsNotDistinctFromFunc parser (IsNotDistinctFrom ref1 ref2) =
    (parser^.parseInfix) "IS NOT DISTINCT FROM" ref1 ref2

-- | Parse "IS NOT FALSE".
parseIsNotFalseFunc :: FuncBoolParser a -> IsNotFalse a -> String
parseIsNotFalseFunc parser (IsNotFalse expr) =
    (parser^.parseIs) expr "NOT FALSE"

-- | Parse "IS NOT NULL".
parseIsNotNullFunc :: FuncBoolParser a -> IsNotNull a -> String
parseIsNotNullFunc parser (IsNotNull expr) = (parser^.parseIs) expr "NOT NULL"

-- | Parse "IS NOT TRUE".
parseIsNotTrueFunc :: FuncBoolParser a -> IsNotTrue a -> String
parseIsNotTrueFunc parser (IsNotTrue expr) =
    (parser^.parseIs) expr "NOT TRUE"

-- | Parse "IS NOT UNKNOWN".
parseIsNotUnknownFunc :: FuncBoolParser a -> IsNotUnknown a -> String
parseIsNotUnknownFunc parser (IsNotUnknown expr) =
    (parser^.parseIs) expr "NOT UNKNOWN"

-- | Parse "IS NULL".
parseIsNullFunc :: FuncBoolParser a -> IsNull a -> String
parseIsNullFunc parser (IsNull expr) = (parser^.parseIs) expr "NULL"

-- | Parse "IS TRUE".
parseIsTrueFunc :: FuncBoolParser a -> IsTrue a -> String
parseIsTrueFunc parser (IsTrue expr) = (parser^.parseIs) expr "TRUE"

-- | Parse "IS UNKNOWN".
parseIsUnknownFunc :: FuncBoolParser a -> IsUnknown a -> String
parseIsUnknownFunc parser (IsUnknown expr) =  (parser^.parseIs) expr "UNKNOWN"

-- | Parse "LIKE".
parseLikeFunc :: FuncBoolParser a -> Like a -> String
parseLikeFunc parser (Like ref1 ref2) = (parser^.parseInfix) "LIKE" ref1 ref2

-- | Parse "NOT BETWEEN".
parseNotBetweenFunc :: FuncBoolParser a -> NotBetween a -> String
parseNotBetweenFunc parser (NotBetween ref lower higher) =
    (parser^.parseBetweens) False ref lower higher

-- | Parse "<>".
parseNotEqualFunc :: FuncBoolParser a -> NotEqual a -> String
parseNotEqualFunc parser (NotEqual ref1 ref2) =
    (parser^.parseInfix) "<>" ref1 ref2

-- | Parse "NOT IN".
parseNotInFunc :: FuncBoolParser a -> NotIn a -> String
parseNotInFunc parser (NotIn ref1 ref2) =
    (parser^.parseInfix) "NOT IN" ref1 ref2

-- | Parse "<".
parseSmallerThanFunc :: FuncBoolParser a -> SmallerThan a -> String
parseSmallerThanFunc parser (SmallerThan ref1 ref2) =
    (parser^.parseInfix) "<" ref1 ref2

-- | Parse "<=".
parseSmallerThanOrEqToFunc :: FuncBoolParser a -> SmallerThanOrEqTo a -> String
parseSmallerThanOrEqToFunc parser (SmallerThanOrEqTo ref1 ref2) =
    (parser^.parseInfix) "<=" ref1 ref2