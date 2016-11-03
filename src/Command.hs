{-# LANGUAGE OverloadedStrings #-}
module Command where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.ByteString

import Character
import Connection
import Item
import Location
import Tangible
import Types

lookupCommand :: String -> [CommandDef] -> Maybe CommandDef
lookupCommand str =
  let
    helper (commandDef@(CommandDef(name, _, _))) = isPrefixOf str name
  in
    find helper

parseCommand :: [CommandDef] -> ByteString -> Either ParseError Command
parseCommand cl input = parse (playerCommand cl) "" input

playerCommand :: [CommandDef] -> GenParser Char st Command
playerCommand cl = do
  cmdName <- word <?> "command"
  case lookupCommand cmdName cl of
    Just cmdDef -> commandArgs cmdDef
    Nothing -> return $ BadCommand $ "You don't know how to \"" `B.append` UTF8.fromString cmdName `B.append` "\"."

commandArgs :: CommandDef -> GenParser Char st Command
commandArgs (CommandDef (name, [], _)) = return $ BadCommand $ "That's not how you " `B.append` UTF8.fromString name `B.append` "."
commandArgs (CommandDef (name, CmdTypeNone:_, f)) = return $ Command (name, CmdArgsNone, f)
commandArgs (CommandDef (name, CmdTypeString:rest, f)) =
  do
    whitespace <?> "whitespace"
    str <- anyString <?> "arg string"
    return $ Command (name, CmdArgsString (UTF8.fromString str), f)
  <|> ( commandArgs (CommandDef (name, rest, f)) )

word :: GenParser Char st String
word = (many1 $ oneOf ['a'..'z']) <?> "word"

anyString :: GenParser Char st String
anyString =
  (many1 $ noneOf "\r\n") <?> "anystring"

whitespace :: GenParser Char st ()
whitespace =
  skipMany1 space <?> "space"

{-
cmdGo :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdGo gs conn (CmdArgsObjectDesc target) = do
  -}

cmdGoDir :: Direction -> GameState -> ClientConnection -> CommandArgs -> IO ()
cmdGoDir dir gs conn _ = do
  let char = connectionCharacter conn
  loc <- atomically $ getLocation char
  destMaybe <- atomically $ findDirDest gs dir loc
  case destMaybe of
    Just dest -> do
      atomically $ do
        locationRemoveObject loc (ObjectCharacter char)
        setContainer char (ContainerLocation dest)
        locationAddObject dest (ObjectCharacter char)
      cmdLook gs conn CmdArgsNone
    Nothing -> hPutStrLn (connectionHandle conn) $ "You can't go " ++ (show dir) ++ " from here!"

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
  loc <- atomically $ getLocation char
  locDesc <- getLocationDesc loc char
  B8.hPutStrLn (connectionHandle conn) locDesc

cmdSay :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdSay gs conn (CmdArgsString str) = do
  let char = connectionCharacter conn
  loc <- atomically $ getLocation char
  sendToRoomExcept loc char "%s says, \"%s\"" [charShortDescription char, \_ -> return str]
  cPutStrLn conn $ "You say, \"" `B.append` str `B.append` "\""

cmdShutdown :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdShutdown gs _ _ =
  atomically $ writeTVar (gameStatus gs) Stopping

cmdCreate :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdCreate gs conn (CmdArgsString tId) = do
  let char = connectionCharacter conn
  loc <- atomically $ getLocation char
  maybeItem <- atomically $ createItem gs (UTF8.toString tId) (ContainerLocation loc)
  case maybeItem of
    Just item -> do
      atomically $ locationAddObject loc (ObjectItem item)
      B8.hPutStrLn (connectionHandle conn) $ "A " `B.append` (itemName item) `B.append` " has just fallen from the sky!"
    Nothing -> B8.hPutStrLn (connectionHandle conn) $ "\"" `B.append` tId `B.append` "\" is not a valid item template ID."

cmdGet :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdGet gs conn (CmdArgsString str) = do
  let char = connectionCharacter conn
  let h = connectionHandle conn
  loc <- atomically $ getLocation char
  maybeItem <- atomically $ findItemAtLocation loc str
  case maybeItem of
    Just item -> do
      maybeSlot <- atomically $ findEmptyHand char
      case maybeSlot of
        Just slot -> do
          atomically $ do
            slotAddItem slot item
            setContainer item (ContainerCharacter char)
            locationRemoveObject loc (ObjectItem item)
          itemDesc <- viewShortDesc item char
          B8.hPutStrLn h $ "You picked up " `B.append` itemDesc `B.append` "."
        Nothing -> hPutStrLn h "You need a free hand to pick that up."
    Nothing -> hPutStrLn h "Could not locate that item here."

getSlotsContentsDescription :: [ItemSlot] -> Character -> IO (Maybe ByteString)
getSlotsContentsDescription slots char = do
  items <- atomically $ fmap concat $ mapM (readTVar . slotContents) slots
  descriptions <- mapM (\t -> viewShortDesc t char) items
  if null descriptions
    then return Nothing
    else return $ Just $ B.intercalate ", " descriptions

cmdInventory :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdInventory gs conn _ = do
  let char = connectionCharacter conn
  let h = connectionHandle conn
  holding <- getSlotsContentsDescription (charHolding char) char
  case holding of
    Just desc -> B8.hPutStrLn h $ "You are holding " `B.append` desc `B.append` "."
    Nothing -> B8.hPutStrLn h $ "You are holding nothing."
  wearing <- getSlotsContentsDescription (charInventory char) char
  case wearing of
    Just desc -> B8.hPutStrLn h $ "You are wearing " `B.append` desc `B.append` "."
    Nothing -> B8.hPutStrLn h $ "You are wearing nothing!"

searchSlots :: ByteString -> [ItemSlot] -> STM (Maybe (Item, ItemSlot))
searchSlots _ [] = return Nothing
searchSlots needle (slot:rest) = do
  result <- fmap (find (\i -> B.isPrefixOf needle (itemName i))) $ readTVar $ slotContents slot
  case result of
    Just item -> return $ Just (item, slot)
    Nothing -> searchSlots needle rest

cmdDrop :: GameState -> ClientConnection -> CommandArgs -> IO ()
cmdDrop gs conn (CmdArgsString str) = do
  let char = connectionCharacter conn
  let h = connectionHandle conn
  results <- atomically $ searchSlots str (charHolding char)
  case results of
    Just (item, slot) -> do
      atomically $ do
        loc <- getLocation char
        slotRemoveItem slot item
        setContainer item (ContainerLocation loc)
        locationAddObject loc (ObjectItem item)
      itemDesc <- viewShortDesc item char
      B8.hPutStrLn h $ "You dropped " `B.append` itemDesc `B.append` "."
    Nothing -> hPutStrLn h $ "You aren't holding that."
