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
  if name == (UTF8.fromString "new")
    then do
      newName <- clientCreateName conn
      password <- clientCreatePassword conn
      atomically $ newCharacter gs newName password
    else do
      -- chars <- atomically $ readTVar $ gameCharacters gs
      mc <- loadCharacter gs (UTF8.toString name)
      case mc of
        Just c -> do
          clientQueryPassword conn c
          return c
        Nothing -> do
          sendToClient conn $ (UTF8.fromString "You must be mistaken. There is no one named \"") `B.append` name `B.append`
              (UTF8.fromString ".\" Let's try this again.\n")
          clientHandshakeChar gs conn

clientQueryName :: Conn -> IO ByteString
clientQueryName conn = do
  sendToClient conn $ UTF8.fromString "Welcome to Dirty Water\x00AE, friend. What name do you go by? If you are new here, type \"new\".\n"
  name <- getFromClientThrow conn
  return $ strTrim name

clientQueryPassword :: Conn -> Character -> IO ()
clientQueryPassword conn char = do
  let name = UTF8.fromString $ charId char
  password <- atomically $ readTVar $ charPassword char
  sendToClient conn $ (UTF8.fromString "Welcome back, ") `B.append` name `B.append`
      (UTF8.fromString ". Please enter your password.\n")
  supposedPassword <- fmap strTrim $ getFromClientThrow conn
  if password == supposedPassword
    then return ()
    else do
      sendToClient conn $ (UTF8.fromString "What are you trying to pull here, \"") `B.append` name `B.append`
          (UTF8.fromString "?\" Maybe I should call you liar instead? Let's start this over.")
      clientQueryPassword conn char

clientCreateName :: Conn -> IO ByteString
clientCreateName conn = do
  sendToClient conn $ UTF8.fromString "Would name would you like to be referred to by?\n"
  name <- fmap strTrim $ getFromClientThrow conn
  sendToClient conn $ (UTF8.fromString "Ah, ") `B.append` name `B.append`
      (UTF8.fromString ", an excellent name! Are you sure that is what you want to go by? (y/n)\n")
  confirmName name
  where
    confirmName :: ByteString -> IO ByteString
    confirmName name = do
      line <- fmap strTrim $ getFromClientThrow conn
      case toString line of
        'y' : _ -> return name
        'n' : _ -> do
          sendToClient conn $ UTF8.fromString "Changed your mind already, eh?\n"
          clientQueryName conn
        _ -> do
          sendToClient conn $ UTF8.fromString "I said, \"(y/n),\" not whatever crap you typed.\n"
          sendToClient conn $ (UTF8.fromString "Let's try this again. Are you sure you want to go by ")
            `B.append` name `B.append` (UTF8.fromString "?\n")
          confirmName name

clientCreatePassword :: Conn -> IO ByteString
clientCreatePassword conn = do
  sendToClient conn $ UTF8.fromString "Please enter a password.\n"
  password <- fmap strTrim $ getFromClientThrow conn
  sendToClient conn $ UTF8.fromString "Please re-enter your password to verify.\n"
  password2 <- fmap strTrim $ getFromClientThrow conn
  if password == password2
    then return password
    else do
      sendToClient conn $ UTF8.fromString "Your passwords did not match. Let's try this again.\n"
      clientCreatePassword conn

getFromClientThrow :: Conn -> IO B.ByteString
getFromClientThrow conn = do
  line <- getFromClient conn
  case line of
    Nothing -> error "No data from client"
    Just l -> return l

