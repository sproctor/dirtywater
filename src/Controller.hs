module Controller
(
  Character(Pc, Npc),
  Connection(Connection, connectionHandle, connectionQueue, connectionCharacter),
  Command(Look, Exit, Move),
  Direction(North, East, South, West),
  GameState(GameState),
  ServerStatus(Running, Stopping),
  addConnection,
  mainServer
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO

data Command = Look | Exit | Move Direction deriving (Show, Eq)

data Direction = North | East | South | West deriving (Show, Eq)

data Character = Pc | Npc deriving (Show, Eq)

data Connection = Connection {
      connectionHandle :: Handle,
      connectionQueue :: TBQueue Command,
      connectionCharacter :: Character
    } deriving Eq

data ServerStatus = Running | Stopping

data GameState = GameState { gameConnections :: TVar [Connection], gameStatus :: ServerStatus }

{- class Controller c where
  getCommand :: c -> IO (Maybe Command)
  putOutput :: c -> String -> IO ()
  getCharacter :: c -> Character

instance Controller Connection where
-}
getCommand :: Connection -> IO (Maybe Command)
getCommand (Connection _ q _) =
  atomically (tryReadTBQueue q)
putOutput :: Connection -> String -> IO ()
putOutput (Connection h _ _) =
  hPutStr h
-- getCharacter :: Connection -> Character
-- getCharacter (Connection _ _ c) = c

mainServer :: IO GameState -> IO ()
mainServer gameState = do
  putStrLn "running server loop"
  gs <- gameState
  conns <- atomically $ readTVar (gameConnections gs)
  let newGameState = processCommands conns gameState
  threadDelay 1000000
  mainServer newGameState

processCommands :: [Connection] -> IO GameState -> IO GameState
processCommands [] gs = gs
processCommands (connection : remainingConnections) gameState = do
  cmd <- getCommand connection
  gs <- gameState
  let
    newGameState =
      case cmd of
        Just realCommand ->
          do
            when (realCommand == Exit) $ atomically $ removeConnection (gameConnections gs) connection
              --print ((getCharacter connection), realCommand)
            doCommand connection realCommand gameState
        Nothing -> gameState
  processCommands remainingConnections newGameState

removeConnection :: TVar [Connection] -> Connection -> STM ()
removeConnection connections connection =
  do
    currentConnections <- readTVar connections
    let newConnections = filter (/= connection) currentConnections
    writeTVar connections newConnections

addConnection :: TVar [Connection] -> Connection -> STM ()
addConnection connections connection =
  do
    currentConnections <- readTVar connections
    writeTVar connections (connection : currentConnections)

doCommand :: Connection -> Command -> IO GameState -> IO GameState
doCommand connection command gs =
  do
    let h = connectionHandle connection
    case command of
      Exit -> do
        hPutStrLn h "Good Bye!"
        hClose h
      Look ->
        hPutStrLn h "There's nothing to see here, move along."
    gs
