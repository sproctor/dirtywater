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

lookupCommand :: String -> [CommandDef] -> Maybe CommandDef
lookupCommand str =
  let
    helper (commandDef@(CommandDef(name, _, _))) = isPrefixOf str name
  in
    find helper

parseCommand :: [CommandDef] -> String -> Either ParseError Command
parseCommand cl input = parse (playerCommand cl) "(unknown)" input

playerCommand :: [CommandDef] -> GenParser Char st Command
playerCommand cl = do
  cmdName <- word <?> "command"
  case lookupCommand cmdName cl of
    Just cmdDef -> commandArgs cmdDef
    Nothing -> return $ BadCommand $ "What are you stupid? You can't " ++ cmdName ++ "."

commandArgs :: CommandDef -> GenParser Char st Command
commandArgs (CommandDef (name, [], _)) = return $ BadCommand $ "That's not how you " ++ name ++ "."
commandArgs (CommandDef (name, CmdTypeNone:_, f)) = return $ Command (name, CmdArgsNone, f)
commandArgs (CommandDef (name, CmdTypeString:rest, f)) =
  do
    whitespace <?> "whitespace"
    str <- anyString <?> "arg string"
    return $ Command (name, CmdArgsString str, f)
  <|> ( commandArgs (CommandDef (name, rest, f)) )

word :: GenParser Char st String
word = (many1 $ oneOf ['a'..'z']) <?> "word"

anyString :: GenParser Char st String
anyString =
  (many1 $ noneOf "\r\n") <?> "anystring"

whitespace :: GenParser Char st ()
whitespace =
  skipMany1 space <?> "space"

cmdExit :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdExit gs conn _ = do
  hPutStrLn (connectionHandle conn) "Good Bye!"
  atomically $ do
    removeConnection (gameClients gs) conn
    writeTVar (connectionClosed conn) True
  tId <- readMVar (connectionThreadId conn)
  throwTo tId ExitException

cmdLook :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdLook gs conn _ = do
  let char = connectionCharacter conn
  locDesc <- atomically $ do
    loc <- getLocation char
    getLocationDesc loc char
  hPutStrLn (connectionHandle conn) locDesc

cmdSay :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdSay gs conn (CmdArgsString str) =
  hPutStrLn (connectionHandle conn) $ "You say, \"" ++ str ++ "\""
