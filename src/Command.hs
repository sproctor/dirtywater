module Command where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Data.List
import System.IO
import Text.ParserCombinators.Parsec

import Connection
import Location
import Tangible
import Types

lookupCommand :: String -> [Command] -> Command
lookupCommand str cl =
  let
    helper [] = BadCommand
    helper (command@(Command (name, _, _)):_) | isPrefixOf str name = command
    helper (_:rest) = helper rest
  in
    helper cl

parseCommand :: GameState -> String -> STM (Either ParseError (Command, CommandArgs))
parseCommand gs input = do
  cl <- readTVar $ commandList gs
  parse (playerCommand gs) "(unknown)" input

playerCommand :: [Command] -> GenParser Char st (Command, CommandArgs)
playerCommand gs = do
  cmdName <- word
  let cmd = lookupCommand cmdName gs
  case cmd of
    Command (_, CmdTypeNoArgs, _) -> return (cmd, CmdNoArgs)
    Command _ -> return (cmd, CmdNoArgs)
    BadCommand -> return (BadCommand, CmdNoArgs)

word :: GenParser Char st String
word = many $ oneOf ['a'..'z']

cmdExit :: GameState -> ClientConnection -> CommandArgs -> STM GameState
cmdExit gs conn _ = do
  liftIO $ hPutStrLn (connectionHandle conn) "Good Bye!"
  removeConnection (gameClients gs) conn
  writeTVar (connectionClosed conn) True
  tId <- readMVar (connectionThreadId conn)
  throwTo tId ExitException

cmdLook :: GameState -> ClientConnection -> CommandArgs -> STM GameState
cmdLook gs conn _ = do
  loc <- getLocation char
  locDesc <- getLocationDesc loc char
  liftIO $ hPutStrLn (connectionHandle conn) locDesc

