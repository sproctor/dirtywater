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

findDirectionPortals :: LuaState -> LocationId -> IO [Object]
findDirectionPortals luaState locId =
  liftM catMaybes $ mapM (getDirectionPortal luaState locId) [(minBound :: Direction) ..]

getDirectionPortal :: LuaState -> LocationId -> Direction -> IO (Maybe Object)
getDirectionPortal luaState locId dir = do
  _ <- Lua.getglobal luaState (show dir)
  isString <- Lua.isstring luaState (-1)
  if isString
    then do
      destId <- Lua.tostring luaState (-1)
      Lua.pop luaState 1
      Lua.pushnil luaState
      Lua.setglobal luaState (show dir)
      return $ Just $ ObjectDirection dir (Portal locId (LocationId (BC.unpack destId)))
    else return Nothing

{-
getLuaGlobalInt :: LuaState -> String -> IO Int
getLuaGlobalInt luaState key = do
  _ <- Lua.getglobal luaState key
  result <- liftM fromIntegral $ Lua.tointeger luaState (-1)
  Lua.pop luaState 1
  -- Set the global to nil so it can't be used accidentally in the future.
  Lua.pushnil luaState
  Lua.setglobal luaState key
  return result
-}

getPropertyForCharacter :: LuaState -> String -> Character -> IO String
getPropertyForCharacter luaState objId _ = do
  _ <- Lua.getglobal luaState "_properties"
  fieldType <- Lua.getfield luaState (-1) objId
  unless (fieldType == Lua.TFUNCTION) $ error $ "Invalid value in _properties[" ++ objId ++ "]"
  -- TODO: put character object on the stack
  Lua.call luaState 0 1
  result <- Lua.tostring luaState (-1)
  Lua.pop luaState 1
  return $ BC.unpack result

getLuaGlobalVisibleProperty :: LuaState -> String -> String -> IO VisibleProperty
getLuaGlobalVisibleProperty luaState objId key = do
  t <- Lua.getglobal luaState key
  case t of
    Lua.TFUNCTION -> do
      propertiesType <- Lua.getglobal luaState "_properties"
      when (propertiesType /= Lua.TTABLE) $ do
        Lua.pop luaState 1
        Lua.newtable luaState
        _ <- Lua.setglobal luaState "_properties"
        void $ Lua.getglobal luaState "_properties"
      Lua.insert luaState (-2)
      Lua.setfield luaState (-2) objId
      Lua.pop luaState 1
      return $ DynamicVisibleProperty (getPropertyForCharacter luaState objId)
    _ -> do
      isString <- Lua.isstring luaState (-1)
      if isString
        then do
          value <- Lua.tostring luaState (-1)
          Lua.pop luaState 1
          return $ StaticVisibleProperty $ BC.unpack value
        else error $ "Property (" ++ key ++ ") has type: " ++ show t

createLocationFromLua :: LuaState -> String -> IO Location
createLocationFromLua luaState objId = do
  let locId = LocationId objId
  title <- getLuaGlobalVisibleProperty luaState objId "title"
  description <- getLuaGlobalVisibleProperty luaState objId "description"
  portals <- findDirectionPortals luaState locId
  objs <- atomically $ newTVar portals
  return $ Location locId title description objs
  
loadLocation :: LuaState -> String -> FilePath -> IO Location
loadLocation luaState baseFileName file = do
  status <- Lua.loadfile luaState file
  if status == Lua.OK
    then do
      Lua.call luaState 0 0
      createLocationFromLua luaState baseFileName
    else do
      errMsg <- Lua.tostring luaState (-1)
      error $ "ERROR: " ++ BC.unpack errMsg

getLocationDesc :: Location -> Character -> IO String
getLocationDesc location char = do
  chars <- atomically $ getLocationChars location
  charDescs <- atomically $ sequence $ map (\ t -> viewShortDesc t char) chars
  let
    charStr = if null chars
      then ""
      else "People here: " ++ (intercalate ", " charDescs) ++ ".\r\n"
  items <- atomically $ getLocationItems location
  itemDescs <- atomically $ sequence $ map (\ t -> viewShortDesc t char) items
  let
    itemStr = if null items
      then ""
      else "Items here: " ++ (intercalate ", " itemDescs) ++ ".\r\n"
  directions <- atomically $ getLocationDirections location
  let directionDescs = map show directions
  let
    directionStr = if null directions
      then ""
      else "Directions here: " ++ (intercalate ", " directionDescs) ++ ".\r\n"
  title <- case locationTitle location of
    DynamicVisibleProperty f -> f char
    StaticVisibleProperty s -> return s
  description <- case locationDescription location of
    DynamicVisibleProperty f -> f char
    StaticVisibleProperty s -> return s
  return $ title ++ "\r\n" ++ description ++ "\r\n" ++ charStr ++ itemStr ++ directionStr

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

