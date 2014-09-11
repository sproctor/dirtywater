{-# LANGUAGE DeriveDataTypeable #-}
module Connection
(
  ClientConnection(ClientConnection, connectionHandle, connectionClosed, connectionCharacter, connectionThreadId),
  ClientConnectionList,
  ExitException(..),
  addConnection,
  isEmptyCommandQueue,
  isExitException,
  getCommand,
  getConnections,
  newConnectionList,
  putOutput,
  removeConnection,
  queueCommand,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Typeable
import System.IO

import Types
import Character

data ClientConnection = ClientConnection {
      connectionHandle :: Handle,
      connectionQueue :: TBQueue Command,
      connectionClosed :: TVar Bool,
      connectionCharacter :: Character,
      connectionThreadId :: MVar ThreadId
    } deriving Eq

data ClientConnectionList = ClientConnectionList { clientConnections :: TVar [ClientConnection] }

data ExitException = ExitException deriving (Show, Typeable)

instance Exception ExitException

isExitException :: ExitException -> Bool
isExitException e =
  typeOf e == typeOf ExitException

getCommand :: ClientConnection -> IO (Maybe Command)
getCommand (ClientConnection _ q _ _ _) =
  atomically (tryReadTBQueue q)

isEmptyCommandQueue :: ClientConnection -> IO Bool
isEmptyCommandQueue conn =
  atomically $ isEmptyTBQueue $ connectionQueue conn

putOutput :: ClientConnection -> String -> IO ()
putOutput (ClientConnection h _ _ _ _) =
  hPutStr h

addConnection :: ClientConnectionList -> ClientConnection -> STM ()
addConnection connections conn =
  do
    currentConnections <- readTVar $ clientConnections connections
    writeTVar (clientConnections connections) (conn : currentConnections)

removeConnection :: ClientConnectionList -> ClientConnection -> STM ()
removeConnection connections conn =
  do
    currentConnections <- readTVar $ clientConnections connections
    let newConnections = filter (/= conn) currentConnections
    writeTVar (clientConnections connections) newConnections

getConnections :: ClientConnectionList -> IO [ClientConnection]
getConnections connectionList =
  atomically $ readTVar $ clientConnections connectionList

newConnectionList :: IO ClientConnectionList
newConnectionList = do
  l <- atomically $ newTVar []
  return $ ClientConnectionList l

queueCommand :: ClientConnection -> Command -> IO ()
queueCommand conn cmd =
  atomically $ writeTBQueue (connectionQueue conn) cmd
