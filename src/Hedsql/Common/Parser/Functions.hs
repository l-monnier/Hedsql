{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hedsql/Common/Parser/Functions.hs
Description : Implementation of the SQL functions parsers.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Implementation of the SQL function parsers.
-}
module Hedsql.Common.Parser.Functions
    (
    -- Interface.
      FuncParser(FuncParser)
    , parseAdd
    , parseBitAnd
    , parseBitOr
    , parseBitShiftLeft
    , parseBitShiftRight
    , parseDivide
    , parseModulo
    , parseMultiply
    , parseSubstract
    , parseCount
    , parseCurrentDate
    , parseMax
    , parseMin
    , parseJoker
    , parseRandom
    , parseSum
    
    -- Dispatch function implementation.
    , parseFuncFunc
    
    -- Generic functions implementation.
    , parseAddFunc
    , parseBitAndFunc
    , parseBitOrFunc
    , parseBitShiftLeftFunc
    , parseBitShiftRightFunc
    , parseDivideFunc
    , parseModuloFunc
    , parseMultiplyFunc
    , parseSubstractFunc
    , parseCountFunc
    , parseCurrentDateFunc
    , parseMaxFunc
    , parseMinFunc
    , parseJokerFunc
    , parseRandomFunc
    , parseSumFunc
    ) where

import Hedsql.Common.DataStructure.Base
import Hedsql.Common.Parser.Queries

import Control.Lens

-- Definition of the parsers interfaces.

{-|
Function parser interface.

Parse all functions which are not returning a boolean value.
Those functions have their own parser.
-}
data FuncParser a = FuncParser
    {
    -- Operators.
      _parseAdd           :: Add a           -> String
    , _parseBitAnd        :: BitAnd a        -> String
    , _parseBitOr         :: BitOr a         -> String
    , _parseBitShiftLeft  :: BitShiftLeft a  -> String
    , _parseBitShiftRight :: BitShiftRight a -> String
    , _parseDivide        :: Divide a        -> String
    , _parseModulo        :: Modulo a        -> String
    , _parseMultiply      :: Multiply a      -> String
    , _parseSubstract     :: Substract a     -> String
    
    -- Functions.
    , _parseCount       :: Count a       -> String
    , _parseCurrentDate :: CurrentDate a -> String
    , _parseMax         :: Max a         -> String
    , _parseMin         :: Min a         -> String
    , _parseJoker       :: Joker a       -> String
    , _parseRandom      :: Random a      -> String
    , _parseSum         :: Sum a         -> String
    
    -- Helper functions
    , _parseInfix :: String -> ColRef a -> ColRef a -> String
    }
    
makeLenses ''FuncParser

-- Helpers functions of the function parser.

-- | Build an expression.
makeExpr :: QueryParser a -> String -> Expression a -> String
makeExpr parser string expression =
    concat [string, "(", parser^.parseExpr $ expression, ")"]

-- Generic implementation of the functions.

-- | Parse a function.
parseFuncFunc :: FuncParser a -> Function a -> String
parseFuncFunc parser func =
    case func of
        -- Operators.
        AddF o           -> parser^.parseAdd $ o
        BitAndF o        -> parser^.parseBitAnd $ o
        BitOrF o         -> parser^.parseBitOr $ o
        BitShiftLeftF o  -> parser^.parseBitShiftLeft $ o
        BitShiftRightF o -> parser^.parseBitShiftRight $ o
        DivideF o        -> parser^.parseDivide $ o
        ModuloF o        -> parser^.parseModulo $ o
        MultiplyF o      -> parser^.parseMultiply $ o
        SubstractF o     -> parser^.parseSubstract $ o 
    
        -- Functions.
        CountF f       -> parser^.parseCount $ f
        CurrentDateF f -> parser^.parseCurrentDate $ f
        JokerF f       -> parser^.parseJoker $ f
        MaxF f         -> parser^.parseMax $ f
        MinF f         -> parser^.parseMin $ f
        RandomF f      -> parser^.parseRandom $ f
        SumF f         -> parser^.parseSum $ f
        
-- | Parse "+".
parseAddFunc :: FuncParser a -> Add a -> String
parseAddFunc parser (Add left right) = (parser^.parseInfix) "+" left right

-- | Parse "&".
parseBitAndFunc :: FuncParser a -> BitAnd a -> String
parseBitAndFunc parser (BitAnd left right) = (parser^.parseInfix) "&" left right

-- | Parse "|".
parseBitOrFunc :: FuncParser a -> BitOr a -> String
parseBitOrFunc parser (BitOr left right) = (parser^.parseInfix) "|" left right

-- | Parse "<<".
parseBitShiftLeftFunc :: FuncParser a -> BitShiftLeft a -> String
parseBitShiftLeftFunc parser (BitShiftLeft left right) =
    (parser^.parseInfix) "<<" left right

-- | Parse ">>".
parseBitShiftRightFunc :: FuncParser a -> BitShiftRight a -> String
parseBitShiftRightFunc parser (BitShiftRight left right) =
    (parser^.parseInfix) ">>" left right

-- | Parse "/".
parseDivideFunc :: FuncParser a -> Divide a -> String
parseDivideFunc parser (Divide left right) = (parser^.parseInfix) "/" left right

-- | Parse "%".
parseModuloFunc :: FuncParser a -> Modulo a -> String
parseModuloFunc parser (Modulo left right) = (parser^.parseInfix) "%" left right

-- | Parse "*" as multiplication operator.
parseMultiplyFunc :: FuncParser a -> Multiply a -> String
parseMultiplyFunc parser (Multiply left right) =
    (parser^.parseInfix) "*" left right

-- | Parse "-".
parseSubstractFunc :: FuncParser a -> Substract a -> String
parseSubstractFunc parser (Substract left right) =
    (parser^.parseInfix)"-" left right 

-- | Parse the CURRENT_DATE function.
parseCurrentDateFunc :: CurrentDate a -> String
parseCurrentDateFunc _ = "CURRENT_DATE"

-- | Build a COUNT function.
parseCountFunc :: QueryParser a -> Count a -> String
parseCountFunc parser (Count expr) = makeExpr parser "COUNT" expr

-- | Build a star character (*).
parseJokerFunc :: Joker a -> String
parseJokerFunc _ = "*"

-- | Parse the MAX function.
parseMaxFunc :: QueryParser a -> Max a -> String
parseMaxFunc parser (Max expr) = makeExpr parser "MAX" expr

-- | Parse the MIN function.
parseMinFunc :: QueryParser a -> Min a -> String
parseMinFunc parser (Min expr) = makeExpr parser "MIN" expr

-- | Parse the RANDOM function.
parseRandomFunc :: Random a -> String
parseRandomFunc _ = "RANDOM"

-- | Build a SUM function.
parseSumFunc :: QueryParser a -> Sum a -> String
parseSumFunc parser (Sum expr) = makeExpr parser "SUM" expr