{-# LANGUAGE OverloadedStrings #-}

module ParseCommand (
  parseCommand
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.List
import Text.Parsec
import Text.Parsec.ByteString
import Types

parseCommand :: [CommandDef] -> ByteString -> Either ParseError Command
parseCommand cl input = parse (playerCommand cl) "" input

playerCommand :: [CommandDef] -> Parser Command
playerCommand cl = do
  cmdName <- word <?> "command"
  case lookupCommand cmdName cl of
    Just cmdDef -> commandArgs cmdDef
    Nothing -> return $ BadCommand $ "You don't know how to \"" `B.append` UTF8.fromString cmdName `B.append` "\"."

commandArgs :: CommandDef -> Parser Command
commandArgs (CommandDef name [] _) =
  return $ BadCommand $ "That's not how you " `B.append` UTF8.fromString name `B.append` "."
commandArgs (CommandDef name (CmdTypeNone:_) f) =
  return $ Command name CmdArgsNone f
commandArgs (CommandDef name (CmdTypeString:rest) f) =
  do
    whitespace <?> "whitespace"
    str <- anyString <?> "arg string"
    return $ Command name (CmdArgsString (UTF8.fromString str)) f
  <|> ( commandArgs (CommandDef name rest f) )

word :: Parser String
word = (many1 $ oneOf ['a'..'z']) <?> "word"

anyString :: Parser String
anyString = (many1 $ noneOf "\r\n") <?> "anystring"

whitespace :: Parser ()
whitespace = skipMany1 space <?> "space"

lookupCommand :: String -> [CommandDef] -> Maybe CommandDef
lookupCommand str = find helper
    where helper (CommandDef name _ _) = isPrefixOf str name

