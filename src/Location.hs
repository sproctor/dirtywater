{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Location where

import Control.Applicative
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Yaml

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

newLocation :: Int -> String -> String -> [Portal] -> STM Location
newLocation id title desc portals = do
  ps <- newTVar portals
  cs <- newTVar []
  is <- newTVar []
  return $ Location id title desc ps cs is

loadLocation :: String -> IO Location
loadLocation file = do
  l <- either (error . show) id <$> decodeFileEither file
  atomically $ newLocation (ldId l) (ldTitle l) (ldDesc l) []

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

