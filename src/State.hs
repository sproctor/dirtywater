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
import Types

processCommands :: GameState -> IO GameState
processCommands gameState =
  let
    processCommand [] gs = return gs
    processCommand (conn : remainingConnections) gs = do
      cmd <- getCommand conn
      updatedGameState <-
        case cmd of
          Just realCommand -> do
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
doCommand conn (command, args) gs =
  do
    let h = connectionHandle conn
    let char = connectionCharacter conn
    case command of
      Command (_, _, f) -> atomically $ f gs conn args
      BadCommand ->
        hPutStrLn h "Bad command! What are you trying to pull?"
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

