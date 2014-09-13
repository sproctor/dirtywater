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

import Command
import Connection
import Location
import Tangible
import Types

processCommands :: GameState -> IO ()
processCommands gameState =
  let
    processCommand [] = return ()
    processCommand (conn : remainingConnections) = do
      mcmd <- atomically $ getCommand conn
      case mcmd of
        Just cmd -> do
          --print ((getCharacter conn), realCommand)
          doCommand conn cmd gameState
          prompt <- atomically $ isEmptyCommandQueue conn
          when prompt $ putOutput conn ">"
        Nothing -> return ()
      processCommand remainingConnections
  in do
    conns <- atomically $ getConnections (gameClients gameState)
    processCommand conns

doCommand :: ClientConnection -> (Command, [CommandArg]) -> GameState -> IO ()
doCommand conn (command, args) gs =
  do
    let h = connectionHandle conn
    let char = connectionCharacter conn
    case command of
      Command (_, _, f) -> f gs conn args
      BadCommand -> do
        hPutStrLn h "Bad command! What are you trying to pull?"

newGameState :: String -> ClientConnectionList -> IO GameState
newGameState dbfilename connections = do
  dbconn <- connectSqlite3 dbfilename
  initDatabase dbconn
  q <- atomically $ newTVar [Command ("look", [], cmdLook), Command ("exit", [], cmdExit)]
  return $ GameState connections Running dbconn q

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (name VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn "Created table"
