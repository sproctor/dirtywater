module Command where

import Text.ParserCombinators.Parsec

import Types

parseCommand :: String -> Either ParseError Command
parseCommand input = parse playerCommand "(unknown)" input

playerCommand :: GenParser Char st Command
playerCommand = do
  cmd <- word
  case cmd of
    "look" -> return $ CmdNoArgs cmd
    _ -> return CmdBadCommand

word :: GenParser Char st String
word = many $ oneOf ['a'..'z']
