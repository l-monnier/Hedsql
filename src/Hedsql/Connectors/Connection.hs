-- file : Hedsql/Connector/Connection.hs
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
    Connection with a database connectivity library allowing to use HESQL
    in a more direct and abstract way.
    
    * "Quick"
    
    Each function performing a query on the database has a "quick" equivalent
    which lift the need perform the connection to the databse.
    This connection is done inside the "quick" functions
    as well as the disconnection.
    
    Those quick functions are directly implemented in the class, so no need
    to re-implement them when creating a new instance.
    
    Examples:
    
    > drop =
    >    runQuick connection dropStatement
    >    where
    >        connection = makeSqLiteConnection "myDatabase.db"
    >        dropStatement = dropTable "films"
    
    Please note that in the above example the changes are directly committed
    to the database and their are therefore no possibilities to perform
    any rollback.
    
    > getOne = fetchOneQuick getConnection $ select (//*) /++ from "films"
-}

module Hedsql.Connectors.Connection (
      module Data.Aeson
    , module Hedsql.Common.Driver
    , module Hedsql.Common.Parser
    , ConnectionBridge
    , commit
    , disconnect
    , fetchOne
    , fetchOneQuick
    , fetchMany
    , fetchManyQuick
    , getTables
    , rollback
    , run
    , runQuick
    ) where

import Data.Aeson
import Hedsql.Common.Driver
import Hedsql.Common.Parser


{- |
 Generic connection bridge allowing to use the HESQL statements to directly
 interact with a database connectivity library such as HBDC.
 
 The connection bridge is however connectivity agnostic in the sense that
 it can potentially be used with any SQL connectivity library.
-}
class ConnectionBridge a where
    
    -- | Commit the changes in the databases.
    commit :: (Driver b) =>
              (a, b) -> IO ()
    
    -- | Disconnect from the database.
    disconnect :: (Driver b) => (a, b) -> IO()
    
    -- | Fetch one row.
    fetchOne :: (Driver b1, Parser b1 b) =>
                (a, b1) -> b -> IO Value 
    
    -- | Fetch one row using a IO driver.
    fetchOneQuick :: (Driver b1, Parser b1 b) =>
                      IO (a, b1) -> b -> IO Value 
    
    -- | Fetch many rows. 
    fetchMany :: (Driver b1, Parser b1 b) =>
                 (a, b1) -> b -> IO Value
    
    -- | Fetch many rows using a IO driver.
    fetchManyQuick :: (Driver b1, Parser b1 b) =>
                       IO (a, b1) -> b -> IO Value
    
    -- | Return all the tables of the database as a list of strings.                 
    getTables :: (Driver b) =>
                 (a, b) -> IO [String]
    
    -- | Quick execution function which will directly commit the query.
    runQuick :: (Driver b1, Parser b1 b) =>
                 IO (a, b1) -> b -> IO Integer
    
    {- |
        Cancel the changes made to the database and come back to
        the previous state.
    -}
    rollback :: (Driver b) => (a, b) -> IO ()
    
    -- | Run a SQL query and return the number of rows modified.          
    run :: (Driver b1, Parser b1 b) =>
           (a, b1) -> b -> IO Integer
    
    -- | Default implementation.
    fetchManyQuick driverIo stmt = do
        c <- driverIo
        result <- fetchMany c stmt
        disconnect c
        return result
    
    -- | Default implementation.
    fetchOneQuick driverIo stmt = do
        c <- driverIo
        result <- fetchOne c stmt
        disconnect c
        return result
    
    -- | Default implementation.      
    runQuick driverIo stmt = do
        c <- driverIo
        result <- run c stmt
        commit c
        disconnect c
        return result