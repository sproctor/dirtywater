{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module UserConnection where

import Control.Concurrent.STM
import Control.Exception
import Data.String.Class
import Data.Typeable
import qualified Data.ByteString as B
import Data.Conduit.Network.Server

import Character
import Types

getCommand :: UserConnection -> STM (Maybe Command)
getCommand uconn = tryReadTBQueue (userCommandQueue uconn)

isEmptyCommandQueue :: UserConnection -> STM Bool
isEmptyCommandQueue user =
  isEmptyTBQueue $ userCommandQueue user

newUserConnection :: GameState -> Conn -> Character -> STM UserConnection
newUserConnection gs conn char = do
  queue <- newTBQueue 10
  closed <- newTVar False
  let user = UserConnection conn queue closed char
  addUserConnection (gameUsers gs) user
  characterConnectionSet char (PlayerConnection user)
  return user

addUserConnection :: UserConnectionList -> UserConnection -> STM ()
addUserConnection usersVar user = do
  users <- readTVar usersVar
  writeTVar usersVar (user : users)

disconnectUser :: GameState -> UserConnection -> IO ()
disconnectUser gs user = do
  atomically $ removeUserConnection (gameUsers gs) user
  disconnectClient (userConnection user)

disconnectCharacter :: GameState -> Character -> IO ()
disconnectCharacter gs char = do
  conn <- atomically $ readTVar (charConnection char)
  case conn of
    PlayerConnection user -> disconnectUser gs user
    NoCharacterConnection -> return ()

removeUserConnection :: UserConnectionList -> UserConnection -> STM ()
removeUserConnection usersVar user = do
  users <- readTVar usersVar
  let updatedUsers = filter (/= user) users
  writeTVar usersVar updatedUsers

newConnectionList :: STM UserConnectionList
newConnectionList = newTVar []

queueCommand :: UserConnection -> Command -> STM ()
queueCommand conn cmd =
  writeTBQueue (userCommandQueue conn) cmd

sendLn :: Conn -> B.ByteString -> IO ()
sendLn conn str = sendToClient conn (B.append str (fromString "\n"))

