{-# LANGUAGE DeriveDataTypeable #-}
module Controller
(
  Character(Pc, Npc),
  ClientConnection(ClientConnection, connectionHandle, connectionQueue, connectionClosed, connectionCharacter, connectionThreadId),
  Command(Look, Exit, Move),
  Direction(North, East, South, West),
  ExitException(ExitException),
  GameState(GameState),
  ServerStatus(Running, Stopping),
  addConnection,
  isExitException,
  mainServer
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Typeable
import Database.HDBC.Sqlite3
import System.IO

data Command = Look | Exit | Move Direction deriving (Show, Eq)

data Direction = North | East | South | West deriving (Show, Eq)

data Character = Pc | Npc deriving (Show, Eq)

data ClientConnection = ClientConnection {
      connectionHandle :: Handle,
      connectionQueue :: TBQueue Command,
      connectionClosed :: TVar Bool,
      connectionCharacter :: Character,
      connectionThreadId :: MVar ThreadId
    } deriving Eq

data ServerStatus = Running | Stopping

data GameState =
  GameState {
    gameConnections :: TVar [ClientConnection],
    gameStatus :: ServerStatus,
    sqlConnection :: Connection
  }

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

mainServer :: GameState -> IO ()
mainServer gameState = do
  putStrLn "running server loop"
  conns <- atomically $ readTVar (gameConnections gameState)
  newGameState <- processCommands conns gameState
  threadDelay 1000000
  mainServer newGameState

processCommands :: [ClientConnection] -> GameState -> IO GameState
processCommands [] gs = return gs
processCommands (conn : remainingConnections) gameState = do
  cmd <- getCommand conn
  newGameState <-
    case cmd of
      Just realCommand -> do
        when (realCommand == Exit) $ atomically $ removeConnection (gameConnections gameState) conn
        --print ((getCharacter conn), realCommand)
        doCommand conn realCommand gameState
      Nothing -> return gameState
  processCommands remainingConnections newGameState

removeConnection :: TVar [ClientConnection] -> ClientConnection -> STM ()
removeConnection connections conn =
  do
    currentConnections <- readTVar connections
    let newConnections = filter (/= conn) currentConnections
    writeTVar connections newConnections

addConnection :: TVar [ClientConnection] -> ClientConnection -> STM ()
addConnection connections conn =
  do
    currentConnections <- readTVar connections
    writeTVar connections (conn : currentConnections)

doCommand :: ClientConnection -> Command -> GameState -> IO GameState
doCommand conn command gs =
  do
    let h = connectionHandle conn
    case command of
      Exit -> do
        hPutStrLn h "Good Bye!"
        atomically $ writeTVar (connectionClosed conn) True
        tId <- readMVar (connectionThreadId conn)
        throwTo tId ExitException
      Look ->
        hPutStrLn h "There's nothing to see here, move along."
      _ ->
        hPutStrLn h "That command is not yet implemented. Sorry."
    return gs
