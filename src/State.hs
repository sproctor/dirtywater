module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import Debug.Trace
import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension, pathSeparator)
import System.IO

import Character
import Command
import Connection
import Helpers
import Item
import Location
import LuaHelpers
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
    , CommandDef ("get", [CmdTypeString], cmdGet)
    , CommandDef ("take", [CmdTypeString], cmdGet)
    , CommandDef ("pickup", [CmdTypeString], cmdGet)
    ]
  status <- atomically $ newTVar Running
  nextIdVar <- atomically $ newTVar 1
  charsTVar <- atomically $ newTVar []
  luaState <- Lua.newstate
  locs <- loadFiles luaState "../data/locations" createLocation
  locTVar <- atomically $ newTVar locs
  itemTemplates <- loadFiles luaState "../data/items" createItemTemplate
  itemTemplatesTVar <- atomically $ newTVar itemTemplates
  let gs = GameState connections status dbconn q locTVar nextIdVar itemTemplatesTVar charsTVar
  loadCharacters gs
  return gs

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (name VARCHAR(255) PRIMARY KEY, containerId VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn "Created characters table"

newCharacter :: GameState -> String -> String -> IO Character
-- newCharacter _ name password | trace ("newCharacter " ++ name ++ " " ++ password) False = undefined
newCharacter gs name password = do
  startLoc <- atomically $ lookupLocation (LocationId "start") gs
  char <- atomically $ createCharacter (ContainerLocation startLoc) name password
  addCharacter gs char
  return char

findCharacter :: String -> GameState -> STM (Maybe Character)
findCharacter name gs = do
  chars <- readTVar (gameCharacters gs)
  findM hasName chars
  where
    hasName :: Character -> STM (Maybe Character)
    hasName c = do
      if (charId c) == name
        then return $ Just c
        else return Nothing

addCharacter :: GameState -> Character -> IO ()
-- addCharacter _ char | trace ("addCharacter gs " ++ show char) False = undefined
addCharacter gs char = do
  let charsTVar = gameCharacters gs
  chars <- atomically $ readTVar charsTVar
  atomically $ writeTVar charsTVar (char : chars)
  let dbconn = sqlConnection gs
  addSqlCharacter dbconn char

writeSqlCharacter :: String -> Connection -> Character -> IO ()
writeSqlCharacter method dbconn char = do
  container <- atomically $ readTVar $ charContainer char
  let
    conId = case container of
      ContainerLocation l -> show $ locationId l
      -- ContainerItem i -> itemId i
  password <- atomically $ readTVar $ charPassword char
  putStrLn $ method ++ " " ++ (charId char) ++ " " ++ conId ++ " " ++ password
  void $ run dbconn (method ++ " INTO characters (name, containerId, password) VALUES (?, ?, ?)")
      [toSql (charId char), toSql conId, toSql password]
  putStrLn $ "Saved character: " ++ show char

addSqlCharacter :: Connection -> Character -> IO ()
addSqlCharacter = writeSqlCharacter "INSERT"

saveCharacter :: Connection -> Character -> IO ()
saveCharacter = writeSqlCharacter "REPLACE"

loadCharacters :: GameState -> IO ()
loadCharacters gs = do
  let dbconn = sqlConnection gs
  stmt <- prepare dbconn "SELECT containerId, name, password FROM characters"
  execute stmt []
  results <- fetchAllRowsAL stmt
  characters <- mapM loadSqlCharacter results
  atomically $ writeTVar (gameCharacters gs) characters
  where
    loadSqlCharacter :: [(String, SqlValue)] -> IO Character
    loadSqlCharacter [("containerId", sId), ("name", sName), ("password", sPassword)] = do
      let conId = fromSql sId
      let name = fromSql sName
      let password = fromSql sPassword
      container <-
        case conId of
          -- TODO: make this prettier
          'l':'o':'c':':' : locId -> do
            loc <- atomically $ lookupLocation (LocationId locId) gs
            return $ ContainerLocation loc
          _ -> throwIO $ InvalidValueException $ "DB column `containerId` must be a location. Value: " ++ conId
      atomically $ createCharacter container name password
      --  Nothing -> fail $ "Non-existant location (" ++ (show conId) ++ ") for character: " ++ name
    loadSqlCharacter x = do
      print x
      fail "Bad result loading Character"
