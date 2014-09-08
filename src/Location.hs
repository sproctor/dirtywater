module Location where

import Control.Concurrent.STM

import Item
import Character
import Types

newLocation :: String -> String -> STM Location
newLocation name desc = do
  ps <- newTVar []
  cs <- newTVar []
  is <- newTVar []
  return $ Location name desc ps cs is
