module Character where

import Control.Concurrent.STM
import Data.List

import Types
import Tangible
import Item

instance Tangible Character where
  getLocation self = do
    con <- readTVar (charContainer self)
    case con of
      ContainerLocation l -> return l
      ContainerItem i -> getLocation i

  getContainer self = readTVar (charContainer self)

  move self = writeTVar (charContainer self)

  matchesDesc self [] name =
    isPrefixOf name (charName self)
  matchesDesc _ _ _ = False
    
  viewShortDesc _ _ = ""
  viewLongDesc _ _ = ""

newCharacter :: String -> Container -> STM Character
newCharacter name con = do
  container <- newTVar con
  return $ Character container name
