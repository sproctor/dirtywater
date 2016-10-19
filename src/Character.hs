module Character where

import Control.Concurrent.STM
import Control.Monad.Extra
import Data.List
import Debug.Trace
import System.IO.Unsafe

import Types
import Tangible
import Item

instance Tangible Character where
  getLocation self = do
    con <- readTVar (charContainer self)
    case con of
      ContainerLocation l -> return l
      ContainerItem i -> getLocation i
      ContainerCharacter i -> getLocation i

  getContainer self = readTVar (charContainer self)

  move self = writeTVar (charContainer self)

  matchesDesc self [] name = do
    -- TODO: matchesDesc should probably take a viewing Character
    description <- charShortDescription self self
    return $ isPrefixOf name description

  matchesDesc _ _ _ = return False

  viewShortDesc _ _ = return ""

  viewLongDesc _ _ = return ""

changePassword :: Character -> String -> STM ()
changePassword self newPassword =
  writeTVar (charPassword self) newPassword

createCharacter :: Container -> String -> String -> STM Character
-- createCharacter container name password | trace ("createCharacter " ++ show container ++ " " ++ name ++ " " ++ password) False = undefined
createCharacter container name password = do
  passwordVar <- newTVar password
  containerVar <- newTVar container
  rContents <- newTVar []
  lContents <- newTVar []
  let hands = [ItemSlot ItemAny rContents, ItemSlot ItemAny lContents]
  stVar <- newTVar 10
  dxVar <- newTVar 10
  iqVar <- newTVar 10
  htVar <- newTVar 10
  hpVar <- newTVar 10
  willVar <- newTVar 10
  perVar <- newTVar 10
  currHPVar <- newTVar 10
  ssVar <- newTVar 2
  skillsVar <- newTVar [Skill "shortsword" ssVar]
  return $ Character name (\_ -> return name) (\_ -> return name) containerVar passwordVar hands [] stVar dxVar iqVar htVar hpVar willVar perVar currHPVar skillsVar

isEmptySlot :: ItemSlot -> STM Bool
isEmptySlot slot = do
  contents <- readTVar (slotContents slot)
  return $ null contents

findEmptyHand :: Character -> STM (Maybe ItemSlot)
findEmptyHand char = do
  findM isEmptySlot (charHolding char)
