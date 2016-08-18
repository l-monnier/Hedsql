{-# LANGUAGE FlexibleContexts  #-}

{-|
Module      : Database/Hedsql/Utils/SqLiteSimple
Description : Use of Hedsql with SQLite-simple package.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Collection of functions allowing a direct use of Hedsql with a SQLite
database through SQLite-Simple library.
-}
module Database.Hedsql.Utils.SqLiteSimple
    ( module Database.SQLite.Simple.FromRow
    , module S
    , module H
    , createTable
    , delete
    , dropTable
    , insert
    , selectOne
    , selectAll
    , update
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Data.Text.Lazy (Text, toStrict)
import           Database.SQLite.Simple.FromRow
import qualified Database.Hedsql.SqLite          as H
import qualified Database.SQLite.Simple          as S

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Execute a statement not returning result.
execute :: H.ToStmt a (H.Statement H.SqLite) => S.Connection -> a -> IO ()
execute conn query = S.execute_ conn $ toQuery $ H.codeGen query

-- | Convert a string to a Query.
toQuery :: Text -> S.Query
toQuery = S.Query . toStrict

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Create a table.
createTable :: S.Connection -> H.Create H.SqLite -> IO ()
createTable = execute

-- | Delete values in a table.
delete :: S.Connection -> H.Delete colType H.SqLite -> IO ()
delete = execute

-- | Drop a table.
dropTable :: S.Connection -> H.Drop H.SqLite -> IO ()
dropTable = execute

-- | Insert values in a table.
insert :: S.Connection -> H.Insert colType H.SqLite -> IO ()
insert = execute

-- | Return the first row of a SELECT query's result.
selectOne :: FromRow r => S.Connection -> H.Select colType H.SqLite -> IO r
selectOne conn query = fmap head $ S.query_ conn $ toQuery $ H.codeGen query

{-|
Return all rows of a SELECT query's result.

Note: since this function uses query_ and not fold_, it should not be used
for big results.
-}
selectAll :: FromRow r => S.Connection -> H.Select colType H.SqLite -> IO [r]
selectAll conn query = S.query_ conn $ toQuery $ H.codeGen query

-- | Update the values of a table.
update :: S.Connection -> H.Update colType H.SqLite -> IO ()
update = execute
