module Character where

import Control.Concurrent.STM
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

createCharacter :: Container -> String -> String -> IO Character
-- createCharacter container name password | trace ("createCharacter " ++ show container ++ " " ++ name ++ " " ++ password) False = undefined
createCharacter container name password = do
  passwordVar <- atomically $ newTVar password
  containerVar <- atomically $ newTVar container
  handsVar <- atomically $ newTVar []
  slotsVar <- atomically $ newTVar []
  invVar <- atomically $ newTVar []
  stVar <- atomically $ newTVar 10
  dxVar <- atomically $ newTVar 10
  iqVar <- atomically $ newTVar 10
  htVar <- atomically $ newTVar 10
  hpVar <- atomically $ newTVar 10
  willVar <- atomically $ newTVar 10
  perVar <- atomically $ newTVar 10
  currHPVar <- atomically $ newTVar 10
  ssVar <- atomically $ newTVar 2
  skillsVar <- atomically $ newTVar [Skill "shortsword" ssVar]
  return $ Character name (\_ -> return name) (\_ -> return name) containerVar passwordVar handsVar slotsVar invVar stVar dxVar iqVar htVar hpVar willVar perVar currHPVar skillsVar
