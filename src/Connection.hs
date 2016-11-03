{-# LANGUAGE DeriveDataTypeable #-}
module Connection where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.String.Class
import Data.Typeable
import System.IO (Handle)

import Types
import Character

data ExitException = ExitException deriving (Show, Typeable)

instance Exception ExitException

isExitException :: ExitException -> Bool
isExitException e =
  typeOf e == typeOf ExitException

getCommand :: ClientConnection -> STM (Maybe Command)
getCommand (ClientConnection _ q _ _ _) =
  tryReadTBQueue q

isEmptyCommandQueue :: ClientConnection -> STM Bool
isEmptyCommandQueue conn =
  isEmptyTBQueue $ connectionQueue conn

cPutStr :: StringRWIO s => ClientConnection -> s -> IO ()
cPutStr (ClientConnection h _ _ _ _) =
  hPutStr h

cPutStrLn :: StringRWIO s => ClientConnection -> s -> IO ()
cPutStrLn (ClientConnection h _ _ _ _) =
  hPutStrLn h

cGetLine :: StringRWIO s => ClientConnection -> IO s
cGetLine (ClientConnection h _ _ _ _) = hGetLine h

newConnection :: GameState -> Handle -> Character -> MVar ThreadId -> STM ClientConnection
newConnection gs h char id = do
  queue <- newTBQueue 10
  closed <- newTVar False
  let conn = ClientConnection h queue closed char id
  addConnection (gameClients gs) conn
  return conn

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

queueCommand :: ClientConnection -> Command -> STM ()
queueCommand conn cmd =
  writeTBQueue (connectionQueue conn) cmd
