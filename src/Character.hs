module Character where

import Control.Concurrent.STM
import Data.List
import System.IO.Unsafe

import Types
import Tangible
import Item

instance Show Character where
  show char =
    let name = unsafePerformIO $ atomically $ readTVar $ charName char in
    "name: " ++ name

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

changeName :: Character -> String -> STM ()
changeName self newName =
  writeTVar (charName self) newName

changePassword :: Character -> String -> STM ()
changePassword self newPassword =
  writeTVar (charPassword self) newPassword

createCharacter :: Container -> String -> String -> IO Character
createCharacter container name password = do
  nameVar <- atomically $ newTVar name
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
  return $ Character containerVar nameVar passwordVar handsVar slotsVar invVar stVar dxVar iqVar htVar hpVar willVar perVar currHPVar skillsVar
