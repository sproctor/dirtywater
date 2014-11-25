module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

import Command
import Connection
import Helpers
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
  charsTVar <- atomically $ newTVar []
  return $ GameState connections Running dbconn q locTVar charsTVar

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (name VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn "Created characters table"

newCharacter :: GameState -> String -> String -> STM Character
newCharacter gs name password = do
  startLoc <- lookupLocation 1001 gs
  case startLoc of
    Just loc -> do
      nameVar <- newTVar name
      passwordVar <- newTVar password
      container <- newTVar (ContainerLocation loc)
      let char = Character container nameVar passwordVar
      addCharacter gs char
      return char
    Nothing -> do
      fail "Start location could not be found!"

findCharacter :: String -> GameState -> STM (Maybe Character)
findCharacter name gs = do
  chars <- readTVar (gameCharacters gs)
  findM hasName chars
  where
    hasName :: Character -> STM (Maybe Character)
    hasName c = do
      cName <- readTVar $ charName c
      if cName == name
        then return $ Just c
        else return Nothing

addCharacter :: GameState -> Character -> STM ()
addCharacter gs char = do
  let charsTVar = gameCharacters gs
  chars <- readTVar charsTVar
  writeTVar charsTVar (char : chars)
