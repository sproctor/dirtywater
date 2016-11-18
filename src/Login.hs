module Login (
  clientHandshakeChar
  ) where

import Prelude hiding (putStrLn)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.String.Class
import Data.Strings
import System.IO (Handle)
import State
import Types

clientHandshakeChar :: GameState -> Handle -> IO Character
clientHandshakeChar gs h = do
  name <- clientQueryName h
  if name == UTF8.fromString "new"
    then do
      newName <- clientCreateName h
      password <- clientCreatePassword h
      atomically $ newCharacter gs h newName password
    else do
      -- chars <- atomically $ readTVar $ gameCharacters gs
      mc <- loadCharacter gs h (UTF8.toString name)
      case mc of
        Just c -> do
          clientQueryPassword h c
          return c
        Nothing -> do
          hPutStrLn h $ "You must be mistaken. There is no one named \"" ++ UTF8.toString name ++ ".\" Let's try this again."
          clientHandshakeChar gs h


clientQueryName :: Handle -> IO ByteString
clientQueryName h = do
  hPutStrLn h "Welcome to Dirty Water\x00AE, friend. What name do you go by? If you are new here, type \"new\"."
  name <- B.hGetLine h
  return $ strTrim name

clientQueryPassword :: Handle -> Character -> IO ()
clientQueryPassword h char = do
  let name = UTF8.fromString $ charId char
  password <- atomically $ readTVar $ charPassword char
  hPutStrLn h $ "Welcome back, " ++ UTF8.toString name ++ ". Please enter your password."
  supposedPassword <- fmap strTrim $ B.hGetLine h
  if password == supposedPassword
    then return ()
    else do
      hPutStrLn h $ "What are you trying to pull here, \"" ++ UTF8.toString name ++ "?\" Maybe I should call you liar instead? Let's start this over."
      clientQueryPassword h char

clientCreateName :: Handle -> IO ByteString
clientCreateName h = do
  hPutStrLn h "Would name would you like to be referred to by?"
  name <- fmap strTrim $ B.hGetLine h
  hPutStrLn h $ "Ah, " ++ UTF8.toString name ++ ", an excellent name! Are you sure that is what you want to go by? (y/n)"
  confirmName name
  where
    confirmName :: ByteString -> IO ByteString
    confirmName name = do
      line <- fmap strTrim $ hGetLine h
      case line of
        'y' : _ -> return name
        'n' : _ -> do
          hPutStrLn h "Changed your mind already, eh?"
          clientQueryName h
        _ -> do
          hPutStrLn h "I said, \"(y/n),\" not whatever crap you typed."
          hPutStrLn h $ "Let's try this again. Are you sure you want to go by " ++ UTF8.toString name ++ "?"
          confirmName name

clientCreatePassword :: Handle -> IO ByteString
clientCreatePassword h = do
  hPutStrLn h "Please enter a password."
  password <- fmap strTrim $ B.hGetLine h
  hPutStrLn h "Please re-enter your password to verify."
  password2 <- fmap strTrim $ B.hGetLine h
  if password == password2
    then return password
    else do
      hPutStrLn h "Your passwords did not match. Let's try this again."
      clientCreatePassword h

