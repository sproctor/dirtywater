{-# LANGUAGE DeriveDataTypeable #-}
module Connection
(
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

data ExitException = ExitException deriving (Show, Typeable)

instance Exception ExitException

isExitException :: ExitException -> Bool
isExitException e =
  typeOf e == typeOf ExitException

getCommand :: ClientConnection -> STM (Maybe (Command, [CommandArg]))
getCommand (ClientConnection _ q _ _ _) =
  tryReadTBQueue q

isEmptyCommandQueue :: ClientConnection -> STM Bool
isEmptyCommandQueue conn =
  isEmptyTBQueue $ connectionQueue conn

putOutput :: ClientConnection -> String -> IO ()
putOutput (ClientConnection h _ _ _ _) =
  hPutStr h

addConnection :: ClientConnectionList -> ClientConnection -> STM ()
addConnection (ClientConnectionList connections) conn =
  do
    currentConnections <- readTVar connections
    writeTVar connections (conn : currentConnections)

removeConnection :: ClientConnectionList -> ClientConnection -> STM ()
removeConnection (ClientConnectionList connections) conn =
  do
    currentConnections <- readTVar connections
    let newConnections = filter (/= conn) currentConnections
    writeTVar connections newConnections

getConnections :: ClientConnectionList -> STM [ClientConnection]
getConnections (ClientConnectionList connectionList) =
  readTVar connectionList

newConnectionList :: STM ClientConnectionList
newConnectionList = do
  l <- newTVar []
  return $ ClientConnectionList l

queueCommand :: ClientConnection -> (Command, [CommandArg]) -> STM ()
queueCommand conn cmd =
  writeTBQueue (connectionQueue conn) cmd
