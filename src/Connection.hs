{-# LANGUAGE DeriveDataTypeable #-}
module Connection
(
  Character(Pc, Npc),
  ClientConnection(ClientConnection, connectionHandle, connectionClosed, connectionCharacter, connectionThreadId),
  ClientConnectionList,
  Command(Look, Exit, Move),
  ExitException(ExitException),
  addConnection,
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

data Command = Look | Exit | Move Direction deriving (Show, Eq)

data Character = Pc | Npc deriving (Show, Eq)

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

{- class Controller c where
  getCommand :: c -> IO (Maybe Command)
  putOutput :: c -> String -> IO ()
  getCharacter :: c -> Character

instance Controller ClientConnection where
-}
getCommand :: ClientConnection -> IO (Maybe Command)
getCommand (ClientConnection _ q _ _ _) =
  atomically (tryReadTBQueue q)
putOutput :: ClientConnection -> String -> IO ()
putOutput (ClientConnection h _ _ _ _) =
  hPutStr h
-- getCharacter :: ClientConnection -> Character
-- getCharacter (ClientConnection _ _ c) = c

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
