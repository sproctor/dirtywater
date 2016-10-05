{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Item where

import Control.Concurrent.STM
import Data.List
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?), (.!=))

import Types
import Tangible

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

  viewShortDesc i _ = return $ (itemTemplShortDesc . itemTemplate) i
  viewLongDesc i _ = return $ (itemTemplLongDesc . itemTemplate) i

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

loadItemTemplate :: FilePath -> IO ItemTemplate
loadItemTemplate file = do
  either (error . show) id <$> Yaml.decodeFileEither file

instance Yaml.FromJSON ItemTemplate where
  parseJSON (Yaml.Object o) = ItemTemplate
    <$> o .: "id"
    <*> o .: "name"
    <*> o .:? "adjs" .!= []
    <*> o .: "sdesc" -- TODO: make this optional and default to adjs + name
    <*> o .: "ldesc" -- TODO: Make this optional and default to sdesc
    <*> o .:? "weapon-type" .!= WeaponNone

instance Yaml.FromJSON WeaponType where
  parseJSON (Yaml.String text) =
    return $ stringToWeaponType (show text)

stringToWeaponType :: String -> WeaponType
stringToWeaponType "shortsword" = WeaponShortsword
stringToWeaponType "broadsword" = WeaponBroadsword
stringToWeaponType _ = WeaponNone

createItem :: GameState -> String -> Container -> STM Item
createItem gs templateId con = do
  template <- lookupTemplate gs templateId
  itemId <- takeItemId gs
  contentsVar <- newTVar []
  conVar <- newTVar con
  return $ Item itemId conVar (\_ -> False) contentsVar template

takeItemId :: GameState -> STM Int
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
