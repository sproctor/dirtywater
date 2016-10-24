{-# LANGUAGE OverloadedStrings #-}
module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8
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
        B8.hPutStrLn h str

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
    , CommandDef ("inventory", [CmdTypeNone], cmdInventory)
    , CommandDef ("drop", [CmdTypeString], cmdDrop)
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
  return gs

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (name VARCHAR(255) PRIMARY KEY, containerId VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn "Created characters table"

newCharacter :: GameState -> Handle -> ByteString -> ByteString -> IO Character
-- newCharacter _ name password | trace ("newCharacter " ++ name ++ " " ++ password) False = undefined
newCharacter gs h name password = do
  startLoc <- atomically $ lookupLocation (LocationId "start") gs
  char <- atomically $ createCharacter (B8.hPutStrLn h) (ContainerLocation startLoc) name password
  addCharacter gs char
  return char

createCharacter :: (ByteString -> IO ()) -> Container -> ByteString -> ByteString -> STM Character
-- createCharacter container name password | trace ("createCharacter " ++ show container ++ " " ++ name ++ " " ++ password) False = undefined
createCharacter msgFun (ContainerLocation loc) name password = do
  passwordVar <- newTVar password
  containerVar <- newTVar (ContainerLocation loc)
  rContents <- newTVar []
  lContents <- newTVar []
  let hands = [ItemSlot ItemAny rContents, ItemSlot ItemAny lContents]
  stVar <- newTVar 10
  dxVar <- newTVar 10
  iqVar <- newTVar 10
  htVar <- newTVar 10
  hpVar <- newTVar 10
  willVar <- newTVar 10
  perVar <- newTVar 10
  currHPVar <- newTVar 10
  ssVar <- newTVar 2
  skillsVar <- newTVar [Skill "shortsword" ssVar]
  let char = Character (UTF8.toString name) msgFun (\_ -> return name) (\_ -> return name) containerVar passwordVar hands [] stVar dxVar iqVar htVar hpVar willVar perVar currHPVar skillsVar
  locationAddObject loc (ObjectCharacter char)
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
  -- putStrLn $ method ++ " " ++ (charId char) ++ " " ++ conId ++ " " ++ password
  void $ run dbconn (method ++ " INTO characters (name, containerId, password) VALUES (?, ?, ?)")
      [toSql (charId char), toSql conId, toSql password]
  -- putStrLn $ "Saved character: " ++ show char

addSqlCharacter :: Connection -> Character -> IO ()
addSqlCharacter = writeSqlCharacter "INSERT"

saveCharacter :: Connection -> Character -> IO ()
saveCharacter = writeSqlCharacter "REPLACE"

loadCharacter :: GameState -> Handle -> String -> IO (Maybe Character)
loadCharacter gs h name = do
  let dbconn = sqlConnection gs
  stmt <- prepare dbconn "SELECT containerId, name, password FROM characters WHERE name = ?"
  execute stmt [toSql name]
  results <- fetchRowAL stmt
  case results of
    Just row -> do
      character <- loadSqlCharacter row
      let charsTVar = gameCharacters gs
      atomically $ do
        characters <- readTVar charsTVar
        writeTVar charsTVar (character : characters)
      return $ Just character
    Nothing -> return Nothing
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
      atomically $ createCharacter (B8.hPutStrLn h) container name password
      --  Nothing -> fail $ "Non-existant location (" ++ (show conId) ++ ") for character: " ++ name
    loadSqlCharacter x = do
      print x
      fail "Bad result loading Character"
