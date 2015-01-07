-- file : Hedsql/Connector/Hdbc/Connection.hs
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
    Connection allowing to use HESQL with HDBC.
    
    * SqLite
    
    You can connect to SqLite using the makeSqLiteConnection function.
    If you do not want to enter the path each time you can create your
    own function such as:
    
    > getConnection = makeSqLiteConnection "myDatabase.db"
    
    When using it, do not forget to lift the IO type, so for example:
    
    > myTest1 = do
    >     c <- getConnection
    >     fetchOne c $ select (//*) /++ from "films"
    
    Alternatively you can use the "quick" equivalent of the functions:
    
    > myTest2 = fetchOneQuick getConnection $ select (//*) /++ from "films"
-}

module Hedsql.Connectors.Hdbc.Connection (
      commit
    , disconnect
    , fetchOne
    , fetchOneQuick
    , fetchMany
    , fetchManyQuick
    , getTables
    , makePostgreSqlConnection
    , makeSqLiteConnection
    , rollback
    , run
    , runQuick
) where

import Data.Map
import Database.HDBC.Sqlite3
import Hedsql.Common.Constructor
import Hedsql.Common.DataStructure
import Hedsql.Common.DefaultParser
import Hedsql.Connectors.Connection
import Hedsql.Drivers.PostgreSQL.Driver
import Hedsql.Drivers.SqLite.Driver 
import Hedsql.Drivers.SqLite.Parser
import Hedsql.Drivers.SqLite.Driver
import Hedsql.Helpers.Json
import Prelude hiding (and)

import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as P
import qualified Database.HDBC.Types as T

-- | HBDC instance.
instance ConnectionBridge Connection where
    commit = H.commit.fst
    
    disconnect = H.disconnect.fst
    
    fetchOne = customedFetch H.fetchRowMap
           
    fetchMany = customedFetch H.fetchAllRowsMap'
    
    getTables = H.getTables.fst      
    
    rollback = H.rollback.fst
    
    run connection stmt = do
        let conn = fst connection
        result <- H.run conn sqlString []
        return result
        where
            sqlString = toSqlString (snd connection) $ stmt

instance ConnectionBridge P.Connection where
    commit = H.commit.fst
    
    disconnect = H.disconnect.fst
    
    fetchOne = customedFetch H.fetchRowMap
           
    fetchMany = customedFetch H.fetchAllRowsMap'
    
    getTables = H.getTables.fst      
    
    rollback = H.rollback.fst
    
    run connection stmt = do
        let conn = fst connection
        result <- H.run conn sqlString []
        return result
        where
            sqlString = toSqlString (snd connection) $ stmt

-- | SQLite connection.
makeSqLiteConnection :: FilePath -> IO (Connection, SqLite)
makeSqLiteConnection path = do
    c <- connectSqlite3 path
    return (c, SqLite)

{- |
    PostgreSQL connection.
    See http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT
    for the meaning of the connection string. 
-}
makePostgreSqlConnection :: String -> IO (P.Connection, PostgreSQL)
makePostgreSqlConnection string = do
    c <- P.connectPostgreSQL string
    return (c, PostgreSQL)

-- TODO: add the connections for other backends.

-- TODO: remove?
-- | Fetch rows using the provided function.
customedFetch fetchFunc connection stmt = do
    let conn = fst connection
    stmt <- H.prepare conn sqlString
    H.execute stmt []
    results <- fetchFunc stmt
    return $ toJSON results
    where
       sqlString = toSqlString (snd connection) $ stmt