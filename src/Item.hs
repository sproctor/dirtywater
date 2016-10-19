{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Item where

import Control.Concurrent.STM
import Data.List
import Scripting.Lua

import Types
import Tangible
import LuaHelpers

instance Tangible Item where
  getLocation self = do
    con <- readTVar (itemContainer self)
    case con of
      ContainerLocation l -> return l
      ContainerItem i -> getLocation i
      ContainerCharacter _ -> error "You are not allowed to have a character inside an item!"

  getContainer self = readTVar (itemContainer self)

  move self = writeTVar (itemContainer self)

  matchesDesc self adjs name =
    return $ isPrefixOf name (itemName self) && findAdjs adjs (itemAdjs self) []

  viewShortDesc i c = showVisibleProperty c $ (itemTemplShortDesc . itemTemplate) i
  viewLongDesc i c = showVisibleProperty c $ (itemTemplLongDesc . itemTemplate) i

itemName :: Item -> String
itemName = itemTemplName . itemTemplate

itemAdjs :: Item -> [String]
itemAdjs = itemTemplAdjs . itemTemplate

canAdd :: Item -> Character -> Bool
canAdd item _ = True
  -- not $ null $ filter (\ (p, _) -> p == pos) (itemContents item)

-- helper functions
findAdjs :: [String] -> [String] -> [String] -> Bool
findAdjs [] _ _ = True
findAdjs _ [] _ = False
findAdjs (prefix:toFind) (adj:toSearch) searched =
  if isPrefixOf prefix adj
    then findAdjs toFind (putTogether searched toSearch) []
    else findAdjs (prefix:toFind) toSearch (adj:searched)

putTogether :: [String] -> [String] -> [String]
putTogether [] xs = xs
putTogether (x:rest) xs = putTogether rest (x:xs)

createItemTemplate :: LuaState -> String -> IO ItemTemplate
createItemTemplate luaState objId = do
  let itemTemplateId = ItemTemplateId objId
  name <- getLuaGlobalString luaState "name"
  shortDescription <- getLuaGlobalVisibleProperty luaState objId "shortDescription"
  longDescription <- getLuaGlobalVisibleProperty luaState objId "longDescription"
  weaponType <- getLuaGlobalString luaState "weaponType"
  return $ ItemTemplate itemTemplateId name [] shortDescription longDescription (read weaponType)
  
createItem :: GameState -> String -> Container -> STM Item
createItem gs templateId con = do
  template <- lookupTemplate gs templateId
  itemId <- takeItemId gs
  contentsVar <- newTVar []
  conVar <- newTVar con
  return $ Item itemId conVar contentsVar template

takeItemId :: GameState -> STM ItemId
takeItemId gs = do
  let idState = gameNextItemId gs
  currId <- readTVar idState
  writeTVar idState (currId + 1)
  return currId

lookupTemplate :: GameState -> String -> STM ItemTemplate
lookupTemplate gs templateId = do
  templates <- readTVar $ gameItemTemplates gs
  case find (\t -> templateId == itemTemplName t) templates of
    Just t -> return t
    Nothing -> error $ "Invalid item template id: " ++ templateId
