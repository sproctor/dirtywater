module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, pathSeparator)
import System.IO

import Character
import Command
import Connection
import Helpers
import Item
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
  initDatabase dbconn
  q <- atomically $ newTVar $
    (map (\d -> CommandDef (dirToString d, [CmdTypeNone], cmdGoDir d)) [North ..]) ++
    [ CommandDef ("look", [CmdTypeNone], cmdLook)
    , CommandDef ("exit", [CmdTypeNone], cmdExit)
    , CommandDef ("quit", [CmdTypeNone], cmdExit)
    , CommandDef ("say", [CmdTypeString], cmdSay)
    , CommandDef ("shutdown", [CmdTypeNone], cmdShutdown)
    , CommandDef ("create", [CmdTypeString], cmdCreate)
    ]
  status <- atomically $ newTVar Running
  nextIdVar <- atomically $ newTVar 1
  locTVar <- atomically $ newTVar []
  itemsTVar <- atomically $ newTVar []
  charsTVar <- atomically $ newTVar []
  let gs = GameState connections status dbconn q locTVar nextIdVar itemsTVar charsTVar
  loadFiles gs "../data/locations" loadLocation
  -- loadFiles gs "../data/items" loadItemTemplate
  loadCharacters gs
  return gs

loadFiles :: GameState -> FilePath -> (GameState -> FilePath -> IO ()) -> IO ()
loadFiles gs path loadFun = do
  files <- getDirectoryContents path
  let luaFiles = filter ((== ".lua") . takeExtension) files
  mapM_ (\f -> loadFun gs (path ++ (pathSeparator : f))) luaFiles

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (conId INTEGER, name VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn "Created characters table"

newCharacter :: GameState -> String -> String -> IO Character
newCharacter gs name password = do
  startLoc <- atomically $ lookupLocation (LocationId 1001) gs
  case startLoc of
    Just loc -> do
      char <- createCharacter (ContainerLocation loc) name password
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

addCharacter :: GameState -> Character -> IO ()
addCharacter gs char = do
  let charsTVar = gameCharacters gs
  chars <- atomically $ readTVar charsTVar
  atomically $ writeTVar charsTVar (char : chars)
  let dbconn = sqlConnection gs
  addSqlCharacter dbconn char

addSqlCharacter :: Connection -> Character -> IO ()
addSqlCharacter dbconn char = do
  container <- atomically $ readTVar $ charContainer char
  let
    id = case container of
      ContainerLocation l -> fromLocationId $ locationId l
      ContainerItem i -> itemId i
  name <- atomically $ readTVar $ charName char
  password <- atomically $ readTVar $ charPassword char
  void $ run dbconn "INSERT INTO characters VALUES (?, ?, ?)" [toSql id, toSql name, toSql password]

loadCharacters :: GameState -> IO ()
loadCharacters gs = do
  let dbconn = sqlConnection gs
  locations <- atomically $ readTVar $ gameLocations gs
  stmt <- prepare dbconn "SELECT conId, name, password FROM characters"
  execute stmt []
  results <- fetchAllRowsAL stmt
  characters <- mapM (loadSqlCharacter locations) results
  atomically $ writeTVar (gameCharacters gs) characters
  where
    loadSqlCharacter :: [Location] -> [(String, SqlValue)] -> IO Character
    loadSqlCharacter locations [("conId", sId), ("name", sName), ("password", sPassword)] = do
      let conId = fromSql sId
      let name = fromSql sName
      let password = fromSql sPassword
      let loc = findLocation (LocationId (fromInteger conId)) locations
      case loc of
        Just l -> createCharacter (ContainerLocation l) name password
        Nothing -> fail $ "Non-existant location (" ++ (show conId) ++ ") for character: " ++ name
    loadSqlCharacter _ x = do
      print x
      fail "Bad result loading Character"
