{-# LANGUAGE OverloadedStrings #-}

module State where

import Prelude hiding (putStrLn)

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.String.Class
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Scripting.Lua as Lua

import Character
import Command
import UserConnection
import Item
import Location
import LuaHelpers
import Skill
import Types

dataDirectory = "data"

processCommands :: GameState -> IO ()
processCommands gameState =
  let
    processCommand [] = return ()
    processCommand (user : remainingUsers) = do
      maybeCommand <- atomically $ getCommand user
      case maybeCommand of
        Just command -> do
          let char = userCharacter user
          -- print ((connectionCharacter conn), cmd)
          catch (doCommand char command gameState)
            (\e -> cPutStrLn char $ "BUG! You've encountered an internal server error: " `B.append` (fromString (show (e :: InvalidValueException))))
          prompt <- atomically $ isEmptyCommandQueue user
          when prompt $ cPutStr char ">"
        Nothing -> return ()
      processCommand remainingUsers
  in do
    users <- atomically $ readTVar $ (gameUsers gameState)
    processCommand users

doCommand :: Character -> Command -> GameState -> IO ()
doCommand char command gs =
    case command of
      Command _ args f -> f gs char args
      BadCommand str -> do
        cPutStrLn char str

newGameState :: String -> UserConnectionList -> IO GameState
newGameState dbfilename users = do
  dbconn <- connectSqlite3 dbfilename
  initDatabase dbconn
  let
    commandList =
      (map (\d -> CommandDef (show d) [CmdTypeNone] (cmdGoDir d)) [(minBound :: Direction) ..]) ++
      [ CommandDef "look" [CmdTypeNone] cmdLook
      , CommandDef "exit" [CmdTypeNone] cmdExit
      , CommandDef "quit" [CmdTypeNone] cmdExit
      , CommandDef "say" [CmdTypeString] cmdSay
      , CommandDef "shutdown" [CmdTypeNone] cmdShutdown
      , CommandDef "create" [CmdTypeList [CmdTypeString, CmdTypeString]] cmdCreate
      , CommandDef "get" [CmdTypeString] cmdGet
      , CommandDef "take" [CmdTypeString] cmdGet
      , CommandDef "pickup" [CmdTypeString] cmdGet
      , CommandDef "inventory" [CmdTypeNone] cmdInventory
      , CommandDef "drop" [CmdTypeString] cmdDrop
      ]
  status <- atomically $ newTVar Running
  nextIdVar <- atomically $ newTVar 1
  nextMobIdVar <- atomically $ newTVar 1
  putStrLn ("Loading Lua files..." :: String)
  luaState <- Lua.newstate
  putStrLn ("Loading locations..." :: String)
  locs <- loadFiles luaState (dataDirectory ++ "/locations") loadLocation
  putStrLn ("Loading item templates..." :: String)
  itemTemplates <- loadFiles luaState (dataDirectory ++ "/items") loadItemTemplate
  putStrLn ("Loading skill definitions..." :: String)
  skillDefs <- loadFiles luaState (dataDirectory ++ "/skills") loadSkillDefinition
  return $ GameState users status dbconn commandList locs nextIdVar nextMobIdVar itemTemplates skillDefs

initDatabase :: Connection -> IO ()
initDatabase dbconn = do
  tables <- getTables dbconn
  unless (elem "characters" tables) $ do
    _ <- run dbconn "CREATE TABLE characters (name VARCHAR(255) PRIMARY KEY, containerId VARCHAR(255), password VARCHAR(255))" []
    commit dbconn
    putStrLn ("Created characters table" ::String)

newCharacter :: GameState -> ByteString -> ByteString -> STM Character
-- newCharacter _ name password | trace ("newCharacter " ++ name ++ " " ++ password) False = undefined
newCharacter gs name password = do
  let startLoc = lookupLocation (LocationId "start") gs
  createCharacter (ContainerLocation startLoc) name password

createCharacter :: Container -> ByteString -> ByteString -> STM Character
-- createCharacter container name password | trace ("createCharacter " ++ show container ++ " " ++ name ++ " " ++ password) False = undefined
createCharacter (ContainerLocation loc) name password = do
  conn <- newTVar NoCharacterConnection
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
  skillsVar <- newTVar [Skill (SkillId "shortsword") 10, Skill (SkillId "broadsword") 10]
  let char = Character (UTF8.toString name) conn (\_ -> return name) (\_ -> return name) containerVar passwordVar hands [] [(Strength, stVar), (Dexterity, dxVar), (Intelligence, iqVar), (Health, htVar)] hpVar skillsVar
  locationAddObject loc (ObjectCharacter char)
  return char

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

loadCharacter :: GameState -> String -> IO (Maybe Character)
loadCharacter gs name = do
  let dbconn = sqlConnection gs
  stmt <- prepare dbconn "SELECT containerId, name, password FROM characters WHERE name = ?"
  execute stmt [toSql name]
  results <- fetchRowAL stmt
  case results of
    Just row -> fmap Just $ loadSqlCharacter row
    Nothing -> return Nothing
  where
    loadSqlCharacter :: [(String, SqlValue)] -> IO Character
    loadSqlCharacter [("containerId", sId), ("name", sName), ("password", sPassword)] = do
      let conId = fromSql sId
      let name = fromSql sName
      let password = fromSql sPassword
      let
        container = case conId of
          'l':'o':'c':':' : locId -> ContainerLocation $ lookupLocation (LocationId locId) gs
          _ -> throw $ InvalidValueException $ "DB column `containerId` must be a location. Value: " ++ conId
      atomically $ createCharacter container name password
      --  Nothing -> fail $ "Non-existant location (" ++ (show conId) ++ ") for character: " ++ name
    loadSqlCharacter x = do
      print x
      fail "Bad result loading Character"
