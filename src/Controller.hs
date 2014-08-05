module Controller
(
  Connection(Connection),
  Character(Pc, Npc),
  addConnection
) where

import Control.Concurrent.STM
import Control.Monad
import System.IO

data Command = Look | Exit | Move Direction deriving (Show, Eq)

data Direction = North | East | South | West deriving (Show, Eq)

data Character = Pc | Npc deriving (Show, Eq)

data Connection = Connection { handle :: Handle, queue :: TBQueue Command, character :: Character } deriving Eq

data ServerStatus = Running | Stopping

data GameState = GameState { status :: ServerStatus }

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

mainServer :: TVar [Connection] -> GameState -> IO ()
mainServer connections gameState = do
  connection <- atomically $ getConnection connections
  cmd <- getCommand connection
  newGameState <-
      case cmd of
        Just realCommand ->
          do
            when (realCommand == Exit) $ atomically $ removeConnection connections connection
              --print ((getCharacter connection), realCommand)
            return $ doCommand connection realCommand gameState
        Nothing -> return gameState
  mainServer connections newGameState

getConnection :: TVar [Connection] -> STM Connection
getConnection connections =
    do
      (connection : remainingConnections) <- readTVar connections
      writeTVar connections (remainingConnections ++ [connection])
      return connection

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

doCommand :: Connection -> Command -> GameState -> GameState
doCommand connection command gs =
  do
    let h = handle connection
    let
      _ =
        case command of
          Exit -> do
            hPutStrLn h "Good Bye!"
            hClose h
          Look ->
            hPutStrLn h "There's nothing to see here, move along."
    gs
