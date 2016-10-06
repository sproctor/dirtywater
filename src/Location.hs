{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Location where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import Foreign.StablePtr
import Foreign.C.Types

import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import qualified Data.ByteString.Char8 as BC

import Character()
import Item()
import Types
import Tangible

data LocationDef =
  LocationDef
    { ldId :: Int
    , ldTitle :: String
    , ldDesc :: String
    , ldPortals :: [PortalDef]
    , ldInitFile :: Maybe String
    }

data PortalDef =
  PortalDef
    { pdDirId :: String
    , pdDestId :: Int
    }

newLocation :: LocationId -> String -> String -> [Object] -> STM Location
newLocation locId title desc portals = do
  objs <- newTVar portals
  return $ Location locId title desc objs

getRequiredField :: LuaState -> String -> IO String
getRequiredField lstate key = do
  _ <- Lua.getfield lstate (-1) key
  str <- Lua.tostring lstate (-1)
  Lua.pop lstate 1
  return $ BC.unpack str

getRequiredFieldInt :: LuaState -> String -> IO Int
getRequiredFieldInt lstate key = do
  _ <- Lua.getfield lstate (-1) key
  i <- Lua.tointeger lstate (-1)
  Lua.pop lstate 1
  return $ fromIntegral i

luaAddLocation :: LuaState -> IO CInt
luaAddLocation luaState = do
  validArg <- Lua.istable luaState (-1)
  when (not validArg) $ error "location argument must be a table"
  locId <- getRequiredFieldInt luaState "id"
  title <- getRequiredField luaState "title"
  description <- getRequiredField luaState "description"
  portals <- findDirectionPortals luaState (LocationId locId)
  _ <- Lua.getglobal luaState "gamestate"
  gsPtr <- Lua.touserdata luaState (-1)
  gs <- deRefStablePtr $ castPtrToStablePtr gsPtr
  addLocation gs (LocationId locId) title description portals
  return 0

findDirectionPortals :: LuaState -> LocationId -> IO [Object]
findDirectionPortals luaState locId = do
  liftM catMaybes $ mapM (getDirectionPortal luaState locId) [(minBound :: Direction) ..]

getDirectionPortal :: LuaState -> LocationId -> Direction -> IO (Maybe Object)
getDirectionPortal luaState locId dir = do
  _ <- Lua.getfield luaState (-1) (show dir)
  val <- Lua.tointegerx luaState (-1)
  Lua.pop luaState 1
  case val of
    Just destId -> return $ Just $ ObjectDirection dir (Portal locId (fromIntegral destId))
    Nothing -> return Nothing

addLocation :: GameState -> LocationId -> String -> String -> [Object] -> IO ()
addLocation gs locId title description objects = do
  objectsVar <- atomically $ newTVar objects
  locs <- atomically $ readTVar (gameLocations gs)
  let loc = Location locId title description objectsVar
  atomically $ writeTVar (gameLocations gs) (loc : locs)

loadLocation :: GameState -> FilePath -> IO ()
loadLocation gs file = do
  luaState <- Lua.newstate
  Lua.openlibs luaState
  gsStablePtr <- newStablePtr gs
  Lua.pushlightuserdata luaState $ castStablePtrToPtr gsStablePtr
  Lua.setglobal luaState "gamestate"
  Lua.registerrawhsfunction luaState "addLocation" luaAddLocation
  status <- Lua.loadfile luaState file
  if status == Lua.OK
    then Lua.call luaState 0 0
    else error $ "Problem processing file \"" ++ file ++ "\""
  Lua.close luaState
  freeStablePtr gsStablePtr

getLocationDesc :: Location -> Character -> STM String
getLocationDesc l char = do
  chars <- getLocationChars l
  charDescs <- sequence $ map (\ t -> viewShortDesc t char) chars
  let
    charStr = if null chars
      then ""
      else "People here: " ++ (intercalate ", " charDescs) ++ ".\r\n"
  items <- getLocationItems l
  itemDescs <- sequence $ map (\ t -> viewShortDesc t char) items
  let
    itemStr = if null items
      then ""
      else "Items here: " ++ (intercalate ", " itemDescs) ++ ".\r\n"
  directions <- getLocationDirections l
  let directionDescs = map show directions
  let
    directionStr = if null directions
      then ""
      else "Directions here: " ++ (intercalate ", " directionDescs) ++ ".\r\n"
  let desc = (locationTitle l) ++ "\r\n" ++ (locationDesc l) ++ "\r\n" ++ charStr ++ itemStr ++ directionStr
  return desc

lookupLocation :: LocationId -> GameState -> STM Location
-- lookupLocation locId _ | trace ("lookupLocation " ++ show locId) False = undefined
lookupLocation locId gs = do
  locations <- readTVar $ gameLocations gs
  case find ((== locId) . locationId) locations of
    Just loc -> return loc
    Nothing -> throwSTM $ InvalidValueException $ "Invalid location ID: " ++ show locId

findDirDest :: GameState -> Direction -> Location -> STM (Maybe Location)
findDirDest gs dir loc = do
  result <- getPortalByDirection loc dir
  case result of
    Just p -> do
      let
        idA = portalDestA p
        idB = portalDestB p
        currId = locationId loc
      destId <-
          if currId == idA
            then return idB
            else
              if currId == idB
                then return idA
                else fail "Bad portal!"
      liftM Just $ lookupLocation destId gs
    Nothing -> return Nothing

getLocationChars :: Location -> STM [Character]
getLocationChars loc = do
  objs <- readTVar $ locationObjects loc
  return $ mapMaybe onlyChars objs
  where
    onlyChars (ObjectCharacter c) = Just c
    onlyChars _ = Nothing

getLocationItems :: Location -> STM [Item]
getLocationItems loc = do
  objs <- readTVar $ locationObjects loc
  return $ mapMaybe onlyItems objs
  where
    onlyItems (ObjectItem i) = Just i
    onlyItems _ = Nothing

getLocationDirections :: Location -> STM [Direction]
getLocationDirections loc = do
  objs <- readTVar $ locationObjects loc
  return $ mapMaybe onlyDirections objs
  where
    onlyDirections (ObjectDirection d _) = Just d
    onlyDirections _ = Nothing

getPortalByDirection :: Location -> Direction -> STM (Maybe Portal)
getPortalByDirection loc dir = do
  objs <- readTVar $ locationObjects loc
  return $ findDir objs
  where
    findDir ((ObjectDirection d p) : rest)
      | d == dir = Just p
      | otherwise = findDir rest
    findDir [] = Nothing
    findDir (_ : rest) = findDir rest

locationAddObject :: Location -> Object -> STM ()
locationAddObject loc obj = do
  let objsVar = locationObjects loc
  objs <- readTVar objsVar
  writeTVar objsVar (obj : objs)

