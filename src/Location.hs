{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Location where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.List
import Data.Maybe

import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua

import Character
import Item
import LuaHelpers
import Types
import Tangible

readDirectionPortals :: LuaState -> LocationId -> IO [Object]
readDirectionPortals luaState locId =
  fmap catMaybes $ mapM (readDirectionPortal luaState locId) [(minBound :: Direction) ..]

readDirectionPortal :: LuaState -> LocationId -> Direction -> IO (Maybe Object)
-- readDirectionPortal _ locId dir | trace ("readDirectionPortal luaState " ++ show locId ++ " " ++ show dir) False = undefined
readDirectionPortal luaState locId dir = do
  _ <- Lua.getglobal luaState (show dir)
  isString <- Lua.isstring luaState (-1)
  if isString
    then do
      destId <- Lua.tostring luaState (-1)
      Lua.pop luaState 1
      Lua.pushnil luaState
      Lua.setglobal luaState (show dir)
      return $ Just $ ObjectDirection dir (Portal locId (LocationId destId))
    else do
      Lua.pop luaState 1
      return Nothing

loadLocation :: LuaState -> String -> IO Location
-- loadLocation _ objId | trace ("loadLocation luaState " ++ objId) False = undefined
loadLocation luaState objId = do
  let locId = LocationId objId
  title <- getLuaGlobalVisibleProperty luaState objId "title"
  description <- getLuaGlobalVisibleProperty luaState objId "description"
  portals <- readDirectionPortals luaState locId
  objs <- atomically $ newTVar portals
  return $ Location locId title description objs
  
getLocationDesc :: Location -> Character -> IO ByteString
getLocationDesc location char = do
  chars <- atomically $ getLocationChars location
  charDescs <- sequence $ map (\ t -> viewShortDesc t char) chars
  let
    charStr = if null chars
      then ""
      else "People here: " `B.append` (B.intercalate ", " charDescs) `B.append` ".\r\n"
  items <- atomically $ getLocationItems location
  itemDescs <- sequence $ map (\ t -> viewShortDesc t char) items
  let
    itemStr = if null items
      then ""
      else "Items here: " `B.append` (B.intercalate ", " itemDescs) `B.append` ".\r\n"
  directions <- atomically $ getLocationDirections location
  let directionDescs = map (UTF8.fromString . show) directions
  let
    directionStr = if null directions
      then ""
      else "Directions here: " `B.append` (B.intercalate ", " directionDescs) `B.append` ".\r\n"
  title <- showVisibleProperty char (locationTitle location)
  description <- showVisibleProperty char (locationDescription location)
  return $ B.concat $ [title, "\r\n", description, "\r\n", charStr, itemStr, directionStr]

lookupLocation :: LocationId -> GameState -> Location
-- lookupLocation locId _ | trace ("lookupLocation " ++ show locId) False = undefined
lookupLocation locId gs = do
  case find ((== locId) . locationId) (gameLocations gs) of
    Just loc -> loc
    Nothing -> throw $ InvalidValueException $ "Invalid location ID: " ++ show locId

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
      return $ Just $ lookupLocation destId gs
    Nothing -> return Nothing

getLocationChars :: Location -> STM [Character]
getLocationChars loc = do
  objs <- readTVar $ locationObjects loc
  return $ mapMaybe onlyChars objs
  where
    onlyChars (ObjectCharacter c) = Just c
    onlyChars _ = Nothing

findCharacterAtLocation :: Location -> ByteString -> STM (Maybe Character)
findCharacterAtLocation loc str = do
  chars <- getLocationChars loc
  return $ find (\char -> B.isPrefixOf str (UTF8.fromString (charId char))) chars

getLocationItems :: Location -> STM [Item]
getLocationItems loc = do
  objs <- readTVar $ locationObjects loc
  return $ mapMaybe onlyItems objs
  where
    onlyItems (ObjectItem i) = Just i
    onlyItems _ = Nothing

findItemAtLocation :: Location -> ByteString -> STM (Maybe Item)
findItemAtLocation loc str = do
  items <- getLocationItems loc
  return $ find (\item -> B.isPrefixOf str (itemName item)) items

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

locationRemoveObject :: Location -> Object -> STM ()
locationRemoveObject loc obj = do
  objs <- readTVar $ locationObjects loc
  writeTVar (locationObjects loc) $ filter ((/=) obj) objs

sendToRoomExcept :: Location -> [Character] -> ByteString -> [Character -> IO ByteString] -> IO ()
sendToRoomExcept loc excluded msg substitutions = do
  chars <- atomically $ getLocationChars loc
  mapM_ (\c -> when (not (elem c excluded)) (sendToCharacter c msg substitutions)) chars
