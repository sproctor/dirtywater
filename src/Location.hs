{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Location where

import Control.Applicative
import Control.Concurrent.STM
import Data.List
import Data.Maybe
import Data.Yaml
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, pathSeparator)

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
    { pdItemId :: Maybe Int
    , pdDirId :: Maybe String
    , pdDestId :: Int
    }

newLocation :: LocationId -> String -> String -> [Portal] -> STM Location
newLocation id title desc portals = do
  ps <- newTVar portals
  cs <- newTVar []
  is <- newTVar []
  return $ Location id title desc ps cs is

loadLocation :: FilePath -> IO Location
loadLocation file = do
  l <- either (error . show) id <$> decodeFileEither file
  atomically $ newLocation (LocationId (ldId l)) (ldTitle l) (ldDesc l) (genPortals (ldPortals l))
  where
    genPortals :: [PortalDef] -> [Portal]
    genPortals ((PortalDef _ (Just dirId) destId) : rest) =
      DirectionPortal (stringToDir dirId) (LocationId destId) : genPortals rest
    genPortals (_ : rest) = genPortals rest
    genPortals [] = []

getLocationDesc :: Location -> Character -> STM String
getLocationDesc l char = do
  chars <- readTVar $ locationChars l
  charDescs <- sequence $ map (\ t -> viewShortDesc t char) chars
  let
    charStr = if null chars
      then ""
      else "People here: " ++ (intercalate ", " charDescs) ++ ".\r\n"
  items <- readTVar $ locationItems l
  itemDescs <- sequence $ map (\ t -> viewShortDesc t char) items
  let
    itemStr = if null items
      then ""
      else "Items here: " ++ (intercalate ", " itemDescs) ++ ".\r\n"
  let desc = (locationTitle l) ++ "\r\n" ++ (locationDesc l) ++ "\r\n" ++ charStr ++ itemStr
  return desc

instance FromJSON LocationDef where
  parseJSON (Object o) = LocationDef
    <$> o .: "id"
    <*> o .: "title"
    <*> o .: "desc"
    <*> o .: "portals"
    <*> o .:? "init_file"
  parseJSON _ = error "Can't parse LocationDef from YAML/JSON"

instance FromJSON PortalDef where
  parseJSON (Object o) = PortalDef
    <$> o .:? "item"
    <*> o .:? "dir"
    <*> o .: "dest"
  parseJSON _ = error "Can't parse PortalDef from YAML/JSON"

loadLocations :: FilePath -> IO [Location]
loadLocations path = do
  files <- getDirectoryContents path
  let yamlFiles = filter ((== ".yaml") . takeExtension) files
  mapM (\f -> loadLocation (path ++ (pathSeparator : f))) yamlFiles

lookupLocation :: LocationId -> GameState -> STM (Maybe Location)
lookupLocation id gs = do
  locations <- readTVar $ gameLocations gs
  return $ findLocation id locations

findLocation :: LocationId -> [Location] -> Maybe Location
findLocation id locations =
  find ((== id) . locationId) locations

findDirDest :: GameState -> Direction -> Location -> STM (Maybe Location)
findDirDest gs dir loc = do
  portals <- readTVar $ locationPortals loc
  let portal = find (portalHasDir dir) portals
  case portal of
    Just (DirectionPortal _ dest) -> lookupLocation dest gs
    _ -> return Nothing

portalHasDir :: Direction -> Portal -> Bool
portalHasDir d (DirectionPortal pd _) =
  pd == d
portalHasDir _ _ = False

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
