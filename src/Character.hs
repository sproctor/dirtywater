{-# LANGUAGE OverloadedStrings #-}

module Character where

import Control.Concurrent.STM
import Control.Monad.Extra
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.List
import Data.Conduit.Network.Server

import Item
import Tangible
import Types

instance Tangible Character where
  getLocation self = do
    con <- readTVar (charContainer self)
    case con of
      ContainerLocation l -> return l
      ContainerItem i -> getLocation i
      ContainerCharacter i -> getLocation i

  getContainer self = readTVar (charContainer self)

  setContainer self con = do
    writeTVar (charContainer self) con

  matchesDesc self [] name = do
    -- TODO: matchesDesc should probably take a viewing Character
    description <- charShortDescription self self
    return $ B.isPrefixOf name description

  matchesDesc _ _ _ = return False

  viewShortDesc = charShortDescription

  viewLongDesc = charLongDescription

changePassword :: Character -> ByteString -> STM ()
changePassword self newPassword =
  writeTVar (charPassword self) newPassword

isEmptySlot :: ItemSlot -> STM Bool
isEmptySlot slot = do
  contents <- readTVar (slotContents slot)
  return $ null contents

findEmptyHand :: Character -> STM (Maybe ItemSlot)
findEmptyHand char =
  findM isEmptySlot (charHolding char)

getHeldItems :: Character -> STM [Item]
getHeldItems char =
  fmap concat $ mapM (readTVar . slotContents) (charHolding char)

getWeapon :: Character -> STM (Maybe Item)
getWeapon char =
  fmap (find isWeapon) (getHeldItems char)

replaceTokens :: ByteString -> [ByteString] -> ByteString
replaceTokens msg [] = msg -- TODO: make sure msg contains no %s
-- replaceTokens msg (substitution:_) | trace ("replaceTokens " ++ UTF8.toString msg ++ "[" ++ UTF8.toString substitution ++ ":...]") False = undefined
replaceTokens msg (substitution:rest) = do
  case B.breakSubstring "%s" msg of
    (head, tail)
      | B.null tail -> msg -- TODO: this is wrong, there are too many subs in the list
      | otherwise -> B.append (B.append head substitution) (replaceTokens (UTF8.drop 2 tail) rest)

sendToCharacter :: Character -> ByteString -> [Character -> IO ByteString] -> IO ()
sendToCharacter char msg substitutions = do
  substitutionStrings <- mapM (\f -> f char) substitutions
  sendToClient (charConn char) (replaceTokens msg substitutionStrings)

getEffectiveSkillRank :: Character -> SkillId -> STM Int
getEffectiveSkillRank char skillId = do
  skills <- readTVar (charSkills char)
  case find (\(Skill sId _) -> skillId == sId) skills of
    Just (Skill _ rank) -> return rank
    Nothing -> getDefaultSkillRank char skillId

getDefaultSkillRank :: Character -> SkillId -> STM Int
getDefaultSkillRank char skillName = return 5
