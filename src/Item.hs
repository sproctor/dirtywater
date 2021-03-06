{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Item where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import Data.List
import Debug.Trace
import Scripting.Lua

import LuaHelpers
import Tangible
import Types

instance Tangible Item where
  getLocation self = do
    con <- readTVar (itemContainer self)
    case con of
      ContainerLocation l -> return l
      ContainerItem i -> getLocation i
      ContainerCharacter _ -> error "You are not allowed to have a character inside an item!"

  getContainer self = readTVar (itemContainer self)

  setContainer self = writeTVar (itemContainer self)

  matchesDesc self adjs name =
    return $ B.isPrefixOf name (itemName self) && findAdjs adjs (itemAdjs self) []

  viewShortDesc i c = showVisibleProperty c $ (itemTemplShortDesc . itemTemplate) i
  viewLongDesc i c = showVisibleProperty c $ (itemTemplLongDesc . itemTemplate) i

itemName :: Item -> ByteString
itemName = itemTemplName . itemTemplate

itemAdjs :: Item -> [ByteString]
itemAdjs = itemTemplAdjs . itemTemplate

canAdd :: Item -> Character -> Bool
canAdd item _ = True
  -- not $ null $ filter (\ (p, _) -> p == pos) (itemContents item)

-- helper functions
findAdjs :: [ByteString] -> [ByteString] -> [ByteString] -> Bool
findAdjs [] _ _ = True
findAdjs _ [] _ = False
findAdjs (prefix:toFind) (adj:toSearch) searched =
  if B.isPrefixOf prefix adj
    then findAdjs toFind (putTogether searched toSearch) []
    else findAdjs (prefix:toFind) toSearch (adj:searched)

putTogether :: [ByteString] -> [ByteString] -> [ByteString]
putTogether [] xs = xs
putTogether (x:rest) xs = putTogether rest (x:xs)

loadItemTemplate :: LuaState -> String -> IO ItemTemplate
loadItemTemplate _ objId | trace ("loadItemTemplate luaState " ++ objId) False = undefined
loadItemTemplate luaState objId = do
  let itemTemplateId = ItemTemplateId objId
  name <- getLuaGlobalString luaState "name"
  shortDescription <- getLuaGlobalVisibleProperty luaState objId "shortDescription"
  longDescription <- getLuaGlobalVisibleProperty luaState objId "longDescription"
  weaponType <- getLuaGlobalStringWithDefault luaState "weaponType" "none"
  return $ ItemTemplate itemTemplateId name [] shortDescription longDescription (SkillId (B.toString weaponType))
  
createItem :: GameState -> String -> Container -> STM (Maybe Item)
createItem gs templateId container = do
  case lookupItemTemplate gs templateId of
    Just template -> do
      itemId <- takeItemId gs
      containerVar <- newTVar container
      return $ Just $ Item itemId containerVar [] template
    Nothing -> return Nothing

takeItemId :: GameState -> STM ItemId
takeItemId gs = do
  let idState = gameNextItemId gs
  currId <- readTVar idState
  writeTVar idState (currId + 1)
  return currId

lookupItemTemplate :: GameState -> String -> Maybe ItemTemplate
lookupItemTemplate gs templateId = do
  find (\t -> (ItemTemplateId templateId) == itemTemplId t) (gameItemTemplates gs)
  --  Nothing -> throwSTM $ "Invalid item template id: " ++ templateId

slotAddItem :: ItemSlot -> Item -> STM ()
slotAddItem slot item = do
  contents <- readTVar (slotContents slot)
  writeTVar (slotContents slot) (item : contents)

slotRemoveItem :: ItemSlot -> Item -> STM ()
slotRemoveItem slot item = do
  contents <- readTVar (slotContents slot)
  writeTVar (slotContents slot) $ filter ((/=) item) contents

itemWeaponType :: Item -> SkillId
itemWeaponType = itemTemplWeaponType . itemTemplate

isWeapon :: Item -> Bool
isWeapon item = (itemWeaponType item) /= (SkillId "none")
