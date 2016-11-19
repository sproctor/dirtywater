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

getCommand :: PlayerConnection -> STM (Maybe Command)
getCommand (PlayerConnection _ q _ _ _) =
  tryReadTBQueue q

isEmptyCommandQueue :: PlayerConnection -> STM Bool
isEmptyCommandQueue conn =
  isEmptyTBQueue $ connectionQueue conn

cPutStr :: StringRWIO s => PlayerConnection -> s -> IO ()
cPutStr (PlayerConnection h _ _ _ _) =
  hPutStr h

cPutStrLn :: StringRWIO s => PlayerConnection -> s -> IO ()
cPutStrLn (PlayerConnection h _ _ _ _) =
  hPutStrLn h

cGetLine :: StringRWIO s => PlayerConnection -> IO s
cGetLine (PlayerConnection h _ _ _ _) = hGetLine h

newConnection :: GameState -> Handle -> Character -> MVar ThreadId -> STM PlayerConnection
newConnection gs h char id = do
  queue <- newTBQueue 10
  closed <- newTVar False
  let conn = PlayerConnection h queue closed char id
  addConnection (gameClients gs) conn
  return conn

addConnection :: PlayerConnectionList -> PlayerConnection -> STM ()
addConnection (PlayerConnectionList connections) conn =
  do
    currentConnections <- readTVar connections
    writeTVar connections (conn : currentConnections)

removeConnection :: PlayerConnectionList -> PlayerConnection -> STM ()
removeConnection (PlayerConnectionList connections) conn =
  do
    currentConnections <- readTVar connections
    let newConnections = filter (/= conn) currentConnections
    writeTVar connections newConnections

getConnections :: PlayerConnectionList -> STM [PlayerConnection]
getConnections (PlayerConnectionList connectionList) =
  readTVar connectionList

newConnectionList :: STM PlayerConnectionList
newConnectionList = do
  l <- newTVar []
  return $ PlayerConnectionList l

queueCommand :: PlayerConnection -> Command -> STM ()
queueCommand conn cmd =
  writeTBQueue (connectionQueue conn) cmd
