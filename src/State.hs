module State
(
  GameState,
  newGameState,
  processCommands
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

import Connection
import Location
import Tangible

data ServerStatus = Running | Stopping

data GameState =
  GameState {
    gameClients :: ClientConnectionList,
    gameStatus :: ServerStatus,
    sqlConnection :: Connection
  }

processCommands :: GameState -> IO GameState
processCommands gameState =
  let
    processCommand [] gs = return gs
    processCommand (conn : remainingConnections) gs = do
      cmd <- getCommand conn
      updatedGameState <-
        case cmd of
          Just realCommand -> do
            when (realCommand == Exit) $ atomically $ removeConnection (gameClients gs) conn
            --print ((getCharacter conn), realCommand)
            newGS <- doCommand conn realCommand gs
            prompt <- isEmptyCommandQueue conn
            when prompt $ putOutput conn ">"
            return newGS
          Nothing -> return gameState
      processCommand remainingConnections updatedGameState
  in do
    conns <- getConnections (gameClients gameState)
    processCommand conns gameState

doCommand :: ClientConnection -> Command -> GameState -> IO GameState
doCommand conn command gs =
  do
    let h = connectionHandle conn
    let char = connectionCharacter conn
    case command of
      Exit -> do
        hPutStrLn h "Good Bye!"
        atomically $ writeTVar (connectionClosed conn) True
        tId <- readMVar (connectionThreadId conn)
        throwTo tId ExitException
      Look -> do
        loc <- atomically $ getLocation char
        locDesc <- atomically $ getLocationDesc loc char
        hPutStrLn h locDesc
      _ ->
        hPutStrLn h "That command is not yet implemented. Sorry."
    return gs

newGameState :: String -> ClientConnectionList -> IO GameState
newGameState dbfilename connections = do
  dbconn <- connectSqlite3 dbfilename
  initDatabase dbconn
  return $ GameState connections Running dbconn

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (name VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn "Created table"
    return ()

