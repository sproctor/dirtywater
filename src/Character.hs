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

  matchesDesc self [] name = do
    myName <- readTVar (charName self)
    return $ isPrefixOf name myName
  matchesDesc _ _ _ = return False

  viewShortDesc _ _ = return ""

  viewLongDesc _ _ = return ""

newCharacter :: String -> Container -> STM Character
newCharacter name con = do
  container <- newTVar con
  newName <- newTVar name
  return $ Character container newName

changeName :: Character -> String -> STM ()
changeName self newName =
  writeTVar (charName self) newName
