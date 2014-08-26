-- file : Hedsql/Common/Driver.hs

{-|
    Driver datatype.
-}

module Hedsql.Common.Driver where

-- | Default SQL driver
data Sql = Sql

class Driver a where
    getName :: a -> String
    
instance Driver Sql where
    getName driver = "SQL"