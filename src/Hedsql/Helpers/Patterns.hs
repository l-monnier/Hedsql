-- file : Hesql/Helpers/Patterns.hs

{-|
   Helper functions related to string patterns used by Hedsql.
-}

module Hedsql.Helpers.Patterns
    (
      generatePattern
    , getPattern
    , getPatternFromList
    , stringIf
    , stringMaybe
    )
    where

-- | Return a pattern which will start by @opening@ and end by @closing@.
--   Between both, the provided @pattern@ will be repeated as many times as @repetitions@.
--   @remove@ lets you specify the number of chars which need to be removed from the last
--   repetition. For example, if the pattern is "test, ", it will be necessary to remove
--   the last 2 characters of the pattern.
getPattern:: [Char] -- ^ Start of the pattern.
          -> [Char] -- ^ End of the pattern.
          -> [Char] -- ^ Pattern which wil be repated and located between the start and the end.
          -> Int  -- ^ Number of times the pattern will be repeated.
          -> Int  -- ^ Number of characters to be removed from the last occurence of the pattern.
          -> [Char] -- ^ The produced pattern.
getPattern opening closing pattern repetitions remove =
    opening ++ (take nbChars (generatePattern pattern)) ++ closing
    where
        nbChars = repetitions*(length pattern)-remove

{-|
    Creates a string pattern from a list.
    
    Imagine you have a list of values such as @["first_name", "last_name"]@ and out of that you
    would like to get the following string: "(first_name, last_name)" you could then use this
    function as so to get that string:
    @getPatternFromList "(" ")" ", " ["first_name", "last_name"]@
    
    Note that this function is polymorphic, so it would be possible for example to creates pattern
    of numbers such as:
    @getPatternFromList [1] [2] [0] [[4], [5]]@
    would return: @[1, 4, 0, 5, 2]@.
-}
getPatternFromList ::
        [a]   -- ^ Start of the pattern. In "(a1, a2)" this would be "(".
    -> [a]   -- ^ End of the pattern. In "(a1, a2)" this woulb be ")".
    -> [a]   -- ^ Separation characters. In "(a1, a2)" this would be ", ".
    -> [[a]] -- ^ Elements to insert in the pattern. In "(a1, a2)" this would be [a1, a2]. 
    -> [a]   -- ^ The returned pattern.
getPatternFromList opening closing separator list =
        opening ++ (take patternLength listPattern) ++ closing
    where
        listPattern = concatenateList separator list
        patternLength = (length listPattern) - (length separator)

concatenateList separator (x:xs) = x ++ separator ++ (concatenateList separator xs)
concatenateList separator [] = []
   

-- | Repeate an infinite number of times the prodivided pattern.
generatePattern:: [a] -- ^ Pattern to repeat.
                        -> [a] -- ^ Pattern repeated an infinite number of times.
generatePattern pattern = pattern ++ (generatePattern pattern)

-- | Return the first string if the Maybe value is Just. Otherwise, return the second string.
stringMaybe :: Maybe a -> String -> String -> String
stringMaybe (Just a) stringJust stringNothing = stringJust
stringMaybe Nothing stringJust stringNothing = stringNothing
         
-- | Return the provided string if the predicate is true. Otherwise, return an empty string.
stringIf :: Bool -> String -> String
stringIf predicate string =
    if predicate
    then string
    else ""