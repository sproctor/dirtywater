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

parseCommand :: [Command] -> String -> Either ParseError (Command, CommandArgs)
parseCommand cl input = parse (playerCommand cl) "(unknown)" input

playerCommand :: [Command] -> GenParser Char st (Command, CommandArgs)
playerCommand cl = do
  cmdName <- word
  let cmd = lookupCommand cmdName cl
  case cmd of
    Command (_, CmdTypeNoArgs, _) -> return (cmd, CmdNoArgs)
    Command _ -> return (cmd, CmdNoArgs)
    BadCommand -> return (BadCommand, CmdNoArgs)

word :: GenParser Char st String
word = many $ oneOf ['a'..'z']

cmdExit :: GameState -> ClientConnection -> CommandArgs -> IO GameState
cmdExit gs conn _ = do
  hPutStrLn (connectionHandle conn) "Good Bye!"
  atomically $ removeConnection (gameClients gs) conn
  atomically $ writeTVar (connectionClosed conn) True
  tId <- readMVar (connectionThreadId conn)
  throwTo tId ExitException
  return gs

cmdLook :: GameState -> ClientConnection -> CommandArgs -> IO GameState
cmdLook gs conn _ = do
  let char = connectionCharacter conn
  loc <- atomically $ getLocation char
  locDesc <- atomically $ getLocationDesc loc char
  hPutStrLn (connectionHandle conn) locDesc
  return gs
