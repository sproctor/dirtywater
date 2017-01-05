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
import Connection
import Item
import Location
import Tangible
import Types

{-
cmdGo :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdGo gs conn (CmdArgsObjectDesc target) = do
  -}

cmdGoDir :: Direction -> GameState -> PlayerConnection -> CommandArgs -> IO ()
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
    Nothing -> cPutStrLn conn $ fromString $ "You can't go " ++ (show dir) ++ " from here!"

cmdExit :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdExit gs pconn _ = do
  cPutStrLn pconn "Good Bye!"
  atomically $ do
    removeConnection (gameClients gs) pconn
    writeTVar (connectionClosed pconn) True
  disconnectClient (connectionConn pconn)

cmdLook :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdLook _gs conn _ = do
  let charecter = connectionCharacter conn
  loc <- atomically $ getLocation charecter
  locDesc <- getLocationDesc loc charecter
  cPutStrLn conn locDesc

cmdSay :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdSay _gs conn (CmdArgsString str) = do
  let char = connectionCharacter conn
  loc <- atomically $ getLocation char
  sendToRoomExcept loc [char] "%s says, \"%s\"" [charShortDescription char, \_ -> return str]
  cPutStrLn conn $ "You say, \"" `B.append` str `B.append` "\""

cmdShutdown :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdShutdown gs _ _ =
  atomically $ writeTVar (gameStatus gs) Stopping

cmdCreate :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdCreate gs conn (CmdArgsString tId) = do
  let char = connectionCharacter conn
  loc <- atomically $ getLocation char
  maybeItem <- atomically $ createItem gs (UTF8.toString tId) (ContainerLocation loc)
  case maybeItem of
    Just item -> do
      atomically $ locationAddObject loc (ObjectItem item)
      cPutStrLn conn $ "A " `B.append` (itemName item) `B.append` " has just fallen from the sky!"
    Nothing -> cPutStrLn conn $ "\"" `B.append` tId `B.append` "\" is not a valid item template ID."

cmdGet :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdGet _gs conn (CmdArgsString str) = do
  let charecter = connectionCharacter conn
  loc <- atomically $ getLocation charecter
  maybeItem <- atomically $ findItemAtLocation loc str
  case maybeItem of
    Just item -> do
      maybeSlot <- atomically $ findEmptyHand charecter
      case maybeSlot of
        Just slot -> do
          atomically $ do
            slotAddItem slot item
            setContainer item (ContainerCharacter charecter)
            locationRemoveObject loc (ObjectItem item)
          itemDesc <- viewShortDesc item charecter
          cPutStrLn conn $ "You picked up " `B.append` itemDesc `B.append` "."
        Nothing -> cPutStrLn conn "You need a free hand to pick that up."
    Nothing -> cPutStrLn conn "Could not locate that item here."

getSlotsContentsDescription :: [ItemSlot] -> Character -> IO (Maybe ByteString)
getSlotsContentsDescription slots char = do
  items <- atomically $ fmap concat $ mapM (readTVar . slotContents) slots
  descriptions <- mapM (\t -> viewShortDesc t char) items
  if null descriptions
    then return Nothing
    else return $ Just $ B.intercalate ", " descriptions

cmdInventory :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdInventory gs conn _ = do
  let char = connectionCharacter conn
  holding <- getSlotsContentsDescription (charHolding char) char
  case holding of
    Just desc -> cPutStrLn conn $ "You are holding " `B.append` desc `B.append` "."
    Nothing -> cPutStrLn conn "You are holding nothing."
  wearing <- getSlotsContentsDescription (charInventory char) char
  case wearing of
    Just desc -> cPutStrLn conn $ "You are wearing " `B.append` desc `B.append` "."
    Nothing -> cPutStrLn conn "You are wearing nothing!"

searchSlots :: ByteString -> [ItemSlot] -> STM (Maybe (Item, ItemSlot))
searchSlots _ [] = return Nothing
searchSlots needle (slot:rest) = do
  result <- fmap (find (\i -> B.isPrefixOf needle (itemName i))) $ readTVar $ slotContents slot
  case result of
    Just item -> return $ Just (item, slot)
    Nothing -> searchSlots needle rest

cmdDrop :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdDrop gs conn (CmdArgsString str) = do
  let char = connectionCharacter conn
  results <- atomically $ searchSlots str (charHolding char)
  case results of
    Just (item, slot) -> do
      atomically $ do
        loc <- getLocation char
        slotRemoveItem slot item
        setContainer item (ContainerLocation loc)
        locationAddObject loc (ObjectItem item)
      itemDesc <- viewShortDesc item char
      cPutStrLn conn $ "You dropped " `B.append` itemDesc `B.append` "."
    Nothing -> cPutStrLn conn "You aren't holding that."

cmdAttack :: GameState -> PlayerConnection -> CommandArgs -> IO ()
cmdAttack gs conn (CmdArgsString str) = do
  let actor = connectionCharacter conn
  loc <- atomically $ getLocation actor
  result <- atomically $ findCharacterAtLocation loc str
  case result of
    Just target -> attackCharacter actor target
    Nothing -> cPutStrLn conn "There is no one by that name here."
