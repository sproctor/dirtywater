module State
(
  GameState,
  newCharacter,
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

doCommand :: ClientConnection -> Command -> GameState -> IO ()
doCommand conn command gs =
  do
    let h = connectionHandle conn
    let char = connectionCharacter conn
    case command of
      Command (_, args, f) -> f gs conn args
      BadCommand str -> do
        hPutStrLn h str

newGameState :: String -> ClientConnectionList -> IO GameState
newGameState dbfilename connections = do
  dbconn <- connectSqlite3 dbfilename
  locations <- loadLocations "../data/locations"
  locTVar <- atomically $ newTVar locations
  initDatabase dbconn
  q <- atomically $ newTVar
    [ CommandDef ("look", [CmdTypeNone], cmdLook)
    , CommandDef ("exit", [CmdTypeNone], cmdExit)
    , CommandDef ("say", [CmdTypeString], cmdSay)
    ]
  return $ GameState connections Running dbconn q locTVar

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (name VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn "Created table"

newCharacter :: GameState -> STM Character
newCharacter gameState = do
  startLoc <- lookupLocation 1001 gameState
  case startLoc of
    Just loc -> do
      newName <- newTVar "New Character"
      container <- newTVar (ContainerLocation loc)
      return $ Character container newName
    Nothing -> do
      fail "Start location could not be found!"
