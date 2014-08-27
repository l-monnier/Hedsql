-- file : Hedsql/Drivers/PostgreSQL/Constructor.hs

{-|
    PostgreSQL specific constructors.
-}

module Hedsql.Drivers.PostgreSQL.Constructor where

import Hedsql.Common.DataStructure.Base

{- |
    DEFAULT instruction when used to insert a DEFAULT value.
    For example:
    > INSERT INTO films Values (DEFAULT, 'Bananas', 88, '1971-07-13', 'Comedy');
-}
-- TODO: find a way to have this value out of the default set of values.
valueDefault = SqlValueDefault