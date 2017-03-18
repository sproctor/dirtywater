{-# LANGUAGE OverloadedStrings #-}

module Command where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.List
import Data.String
import Data.Conduit.Network.Server

import Character
import Combat
import Item
import Location
import Tangible
import Types
import UserConnection

{-
cmdGo :: GameState -> Character -> CommandArgs -> IO ()
cmdGo gs user (CmdArgsObjectDesc target) = do
  -}

-- Template for commands: north, south, etc.
cmdGoDir :: Direction -> GameState -> Character -> CommandArgs -> IO ()
cmdGoDir dir gs char _ = do
  loc <- atomically $ getLocation char
  destMaybe <- atomically $ findDirDest gs dir loc
  case destMaybe of
    Just dest -> do
      atomically $ do
        locationRemoveObject loc (ObjectCharacter char)
        setContainer char (ContainerLocation dest)
        locationAddObject dest (ObjectCharacter char)
      cmdLook gs char CmdArgsNone
    Nothing -> cPutStrLn char $ fromString $ "You can't go " ++ (show dir) ++ " from here!"

cmdExit :: GameState -> Character -> CommandArgs -> IO ()
cmdExit gs char _ = do
  cPutStrLn char "Good Bye!"
  disconnectCharacter gs char

cmdLook :: GameState -> Character -> CommandArgs -> IO ()
cmdLook _gs char _ = do
  loc <- atomically $ getLocation char
  locDesc <- getLocationDesc loc char
  cPutStrLn char locDesc

cmdSay :: GameState -> Character -> CommandArgs -> IO ()
cmdSay _gs char (CmdArgsString str) = do
  loc <- atomically $ getLocation char
  sendToRoomExcept loc [char] "%s says, \"%s\"" [charShortDescription char, \_ -> return str]
  cPutStrLn char $ "You say, \"" `B.append` str `B.append` "\""

cmdShutdown :: GameState -> Character -> CommandArgs -> IO ()
cmdShutdown gs _ _ =
  atomically $ writeTVar (gameStatus gs) Stopping

-- God command for: items
cmdCreate :: GameState -> Character -> CommandArgs -> IO ()
cmdCreate gs char (CmdArgsList [(CmdArgsString "item"), (CmdArgsString tId)]) = do
  loc <- atomically $ getLocation char
  maybeItem <- atomically $ createItem gs (UTF8.toString tId) (ContainerLocation loc)
  case maybeItem of
    Just item -> do
      atomically $ locationAddObject loc (ObjectItem item)
      cPutStrLn char $ "A " `B.append` (itemName item) `B.append` " has just fallen from the sky!"
    Nothing -> cPutStrLn char $ "\"" `B.append` tId `B.append` "\" is not a valid item template ID."
cmdCreate gs char (CmdArgsList [(CmdArgsString "mob"), (CmdArgsString tId)]) = do
  cPutStrLn char $ "Function not yet implemented."

cmdGet :: GameState -> Character -> CommandArgs -> IO ()
cmdGet _gs char (CmdArgsString str) = do
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
          cPutStrLn char $ "You picked up " `B.append` itemDesc `B.append` "."
        Nothing -> cPutStrLn char "You need a free hand to pick that up."
    Nothing -> cPutStrLn char "Could not locate that item here."

getSlotsContentsDescription :: [ItemSlot] -> Character -> IO (Maybe ByteString)
getSlotsContentsDescription slots char = do
  items <- atomically $ fmap concat $ mapM (readTVar . slotContents) slots
  descriptions <- mapM (\t -> viewShortDesc t char) items
  if null descriptions
    then return Nothing
    else return $ Just $ B.intercalate ", " descriptions

cmdInventory :: GameState -> Character -> CommandArgs -> IO ()
cmdInventory gs char _ = do
  holding <- getSlotsContentsDescription (charHolding char) char
  case holding of
    Just desc -> cPutStrLn char $ "You are holding " `B.append` desc `B.append` "."
    Nothing -> cPutStrLn char "You are holding nothing."
  wearing <- getSlotsContentsDescription (charInventory char) char
  case wearing of
    Just desc -> cPutStrLn char $ "You are wearing " `B.append` desc `B.append` "."
    Nothing -> cPutStrLn char "You are wearing nothing!"

searchSlots :: ByteString -> [ItemSlot] -> STM (Maybe (Item, ItemSlot))
searchSlots _ [] = return Nothing
searchSlots needle (slot:rest) = do
  result <- fmap (find (\i -> B.isPrefixOf needle (itemName i))) $ readTVar $ slotContents slot
  case result of
    Just item -> return $ Just (item, slot)
    Nothing -> searchSlots needle rest

cmdDrop :: GameState -> Character -> CommandArgs -> IO ()
cmdDrop gs char (CmdArgsString str) = do
  results <- atomically $ searchSlots str (charHolding char)
  case results of
    Just (item, slot) -> do
      atomically $ do
        loc <- getLocation char
        slotRemoveItem slot item
        setContainer item (ContainerLocation loc)
        locationAddObject loc (ObjectItem item)
      itemDesc <- viewShortDesc item char
      cPutStrLn char $ "You dropped " `B.append` itemDesc `B.append` "."
    Nothing -> cPutStrLn char "You aren't holding that."

cmdAttack :: GameState -> Character -> CommandArgs -> IO ()
cmdAttack gs actor (CmdArgsString str) = do
  loc <- atomically $ getLocation actor
  result <- atomically $ findCharacterAtLocation loc str
  case result of
    Just target -> attackCharacter actor target
    Nothing -> cPutStrLn actor "There is no one by that name here."
