module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
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
          -- print ((connectionCharacter conn), cmd)
          catch (doCommand conn cmd gameState)
            (\e -> putOutput conn $ "BUG! You've encountered an internal server error: " ++ (show (e :: InvalidValueException)) ++ "\r\n")
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
    (map (\d -> CommandDef (show d, [CmdTypeNone], cmdGoDir d)) [(minBound :: Direction) ..]) ++
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
  char <- createCharacter (ContainerLocation startLoc) name password
  addCharacter gs char
  return char

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
      ContainerLocation l -> fromIntegral $ locationId l
      ContainerItem i -> itemId i
  name <- atomically $ readTVar $ charName char
  password <- atomically $ readTVar $ charPassword char
  void $ run dbconn "INSERT INTO characters VALUES (?, ?, ?)" [toSql id, toSql name, toSql password]

loadCharacters :: GameState -> IO ()
loadCharacters gs = do
  let dbconn = sqlConnection gs
  stmt <- prepare dbconn "SELECT conId, name, password FROM characters"
  execute stmt []
  results <- fetchAllRowsAL stmt
  characters <- mapM loadSqlCharacter results
  atomically $ writeTVar (gameCharacters gs) characters
  where
    loadSqlCharacter :: [(String, SqlValue)] -> IO Character
    loadSqlCharacter [("conId", sId), ("name", sName), ("password", sPassword)] = do
      let conId = fromSql sId
      let name = fromSql sName
      let password = fromSql sPassword
      loc <- atomically $ lookupLocation (LocationId (fromInteger conId)) gs
      createCharacter (ContainerLocation loc) name password
      --  Nothing -> fail $ "Non-existant location (" ++ (show conId) ++ ") for character: " ++ name
    loadSqlCharacter x = do
      print x
      fail "Bad result loading Character"
