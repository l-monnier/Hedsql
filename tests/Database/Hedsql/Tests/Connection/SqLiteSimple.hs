{-|
Module      : Database/Hedsql/Tests/Connection/SqLiteSimple
Description : Test statements with the SqLite Simple connection.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of tests for the SqLite Simple connection.
-}
module Database.Hedsql.Tests.Connection.SqLiteSimple
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Statements.Create
import Database.Hedsql.Statements.Drop

import Database.Hedsql.Utils.SqLiteSimple
import Database.SQLite.Simple
import Test.Framework                  (Test, testGroup)
import Test.Framework.Providers.HUnit  (testCase)

import qualified Database.Hedsql.SqLite          as H

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Name of the database used for the tests.
dbName :: String
dbName = "test.db"

-- | SqLite CREATE function.
sqLiteCreate :: H.CreateStmt H.SqLite -> IO ()
sqLiteCreate f = do
    conn <- open dbName
    createTable conn f

-- | SqLite INSERT function.
{-
sqLiteInsert :: H.Insert H.SqLite -> IO ()
sqLiteInsert f = do
    conn <- open dbName
    insertTable conn f
-}

-- | SqLite DROP function.
sqLiteDrop :: H.Drop H.SqLite -> IO ()
sqLiteDrop f = do
    conn <- open dbName
    dropTable conn f

testDropIfExists :: (H.Drop a -> IO ()) -> Test
testDropIfExists f =
    testCase "Drop a table if already existing" $ f dropTableIfExistsStmt

testCreate :: (H.CreateStmt a -> IO ()) -> Test
testCreate f = testCase "Create a simple table" $ f simpleTable

{-
testDefaultVal :: (H.CreateStmt a -> IO ()) -> Test
testDefaultVal createFunc insertFunc =
    testCase "Create a table with a default value" $ assertCreate
    where
        assertCreate = do
            createFunc defaultVal
            insertFunc $ insert "People" [H.assign "country"]
-}

-- | Create the tests for a given DB vendor.
makeTests :: String -> ((H.CreateStmt a -> IO ()), (H.Drop a -> IO ())) -> Test
makeTests name (createFunc, dropFunc) = testGroup name
    [ -- Tests initialisation.
      testDropIfExists dropFunc
    , testCreate createFunc

      -- Tests cleaning
    , testDropIfExists dropFunc
    ]

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = makeTests "SqLite Simple Connection" (sqLiteCreate, sqLiteDrop)
