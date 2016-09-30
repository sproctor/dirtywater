{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Location where

import Control.Applicative
import Control.Concurrent.STM
import Data.List
import Data.Maybe
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?))

import Character
import Item
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
newLocation id title desc portals = do
  objs <- newTVar portals
  return $ Location id title desc objs

addLocation :: LuaState -> IO CInt
addLocation id locationMap = do
  

loadLocation :: FilePath -> IO Location
loadLocation file = do
  luaState <- Lua.newstate
  Lua.openlibs luaState
  Lua.register luaState "addLocation" addLocation
  Lua.dofile luaState file

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
  let directionDescs = map dirToString directions
  let
    directionStr = if null directions
      then ""
      else "Directions here: " ++ (intercalate ", " directionDescs) ++ ".\r\n"
  let desc = (locationTitle l) ++ "\r\n" ++ (locationDesc l) ++ "\r\n" ++ charStr ++ itemStr ++ directionStr
  return desc

instance Yaml.FromJSON LocationDef where
  parseJSON (Yaml.Object o) = LocationDef
    <$> o .: "id"
    <*> o .: "title"
    <*> o .: "desc"
    <*> o .: "portals"
    <*> o .:? "init_file"
  parseJSON _ = error "Can't parse LocationDef from YAML/JSON"

instance Yaml.FromJSON PortalDef where
  parseJSON (Yaml.Object o) = PortalDef
    <$> o .: "dir"
    <*> o .: "dest"
  parseJSON _ = error "Can't parse PortalDef from YAML/JSON"

lookupLocation :: LocationId -> GameState -> STM (Maybe Location)
lookupLocation id gs = do
  locations <- readTVar $ gameLocations gs
  return $ findLocation id locations

findLocation :: LocationId -> [Location] -> Maybe Location
findLocation id locations =
  find ((== id) . locationId) locations

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
      lookupLocation destId gs
    Nothing -> return Nothing

fromLocationId :: LocationId -> Int
fromLocationId (LocationId id) = id

stringToDir :: String -> Direction
stringToDir "north" = North
stringToDir "northeast" = Northeast
stringToDir "east" = East
stringToDir "southeast" = Southeast
stringToDir "south" = South
stringToDir "southwest" = Southwest
stringToDir "west" = West
stringToDir "northwest" = Northwest

dirToString :: Direction -> String
dirToString North = "north"
dirToString Northeast = "northeast"
dirToString East = "east"
dirToString Southeast = "southeast"
dirToString South = "south"
dirToString Southwest = "southwest"
dirToString West = "west"
dirToString Northwest = "northwest"

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

