module Location where

import Control.Concurrent.STM
import Data.List

import Character
import Item
import Types
import Tangible

newLocation :: String -> String -> STM Location
newLocation name desc = do
  ps <- newTVar []
  cs <- newTVar []
  is <- newTVar []
  return $ Location name desc ps cs is

getLocationDesc :: Location -> Character -> STM String
getLocationDesc l char = do
  chars <- readTVar $ locationChars l
  charDescs <- viewShortDescs chars char
  let
    charStr = if null chars
      then ""
      else "People here: " ++ (intercalate ", " charDescs) ++ ".\r\n"
  items <- readTVar $ locationItems l
  itemDescs <- viewShortDescs items char
  let
    itemStr = if null items
      then ""
      else "Items here: " ++ (intercalate ", " itemDescs) ++ ".\r\n"
  let desc = (locationTitle l) ++ "\r\n" ++ (locationDesc l) ++ "\r\n" ++ charStr ++ itemStr
  return desc
