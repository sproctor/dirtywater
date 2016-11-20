{-# LANGUAGE OverloadedStrings #-}

module Login (
  clientHandshakeChar
  ) where

import Prelude hiding (putStrLn)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.String.Class
import Data.Strings hiding (toString)
import Data.Conduit.Network.Server
import State
import Types

clientHandshakeChar :: GameState -> Conn -> IO Character
clientHandshakeChar gs conn = do
  name <- clientQueryName conn
  if name == "new"
    then do
      newName <- clientCreateName conn
      password <- clientCreatePassword conn
      atomically $ newCharacter gs conn newName password
    else do
      -- chars <- atomically $ readTVar $ gameCharacters gs
      mc <- loadCharacter gs conn (UTF8.toString name)
      case mc of
        Just c -> do
          clientQueryPassword conn c
          return c
        Nothing -> do
          sendToClient conn $ "You must be mistaken. There is no one named \"" `B.append` name `B.append` ".\" Let's try this again."
          clientHandshakeChar gs conn

clientQueryName :: Conn -> IO ByteString
clientQueryName conn = do
  sendToClient conn "Welcome to Dirty Water\x00AE, friend. What name do you go by? If you are new here, type \"new\"."
  name <- getFromClientThrow conn
  return $ strTrim name

clientQueryPassword :: Conn -> Character -> IO ()
clientQueryPassword conn char = do
  let name = UTF8.fromString $ charId char
  password <- atomically $ readTVar $ charPassword char
  sendToClient conn $ "Welcome back, " `B.append` name `B.append` ". Please enter your password."
  supposedPassword <- fmap strTrim $ getFromClientThrow conn
  if password == supposedPassword
    then return ()
    else do
      sendToClient conn $ "What are you trying to pull here, \"" `B.append` name `B.append` "?\" Maybe I should call you liar instead? Let's start this over."
      clientQueryPassword conn char

clientCreateName :: Conn -> IO ByteString
clientCreateName conn = do
  sendToClient conn "Would name would you like to be referred to by?"
  name <- fmap strTrim $ getFromClientThrow conn
  sendToClient conn $ "Ah, " `B.append` name `B.append` ", an excellent name! Are you sure that is what you want to go by? (y/n)"
  confirmName name
  where
    confirmName :: ByteString -> IO ByteString
    confirmName name = do
      line <- fmap strTrim $ getFromClientThrow conn
      case toString line of
        'y' : _ -> return name
        'n' : _ -> do
          sendToClient conn "Changed your mind already, eh?"
          clientQueryName conn
        _ -> do
          sendToClient conn "I said, \"(y/n),\" not whatever crap you typed."
          sendToClient conn $ "Let's try this again. Are you sure you want to go by " `B.append` name `B.append` "?"
          confirmName name

clientCreatePassword :: Conn -> IO ByteString
clientCreatePassword conn = do
  sendToClient conn "Please enter a password."
  password <- fmap strTrim $ getFromClientThrow conn
  sendToClient conn "Please re-enter your password to verify."
  password2 <- fmap strTrim $ getFromClientThrow conn
  if password == password2
    then return password
    else do
      sendToClient conn "Your passwords did not match. Let's try this again."
      clientCreatePassword conn

getFromClientThrow :: Conn -> IO B.ByteString
getFromClientThrow conn = do
  line <- getFromClient conn
  case line of
    Nothing -> error "No data from client"
    Just l -> return l

