module Combat where

import Control.Concurrent.STM

import Character
import Item
import Roll
import Types

attackCharacter :: Character -> Character -> IO ()
attackCharacter actor target = do
  weaponMaybe <- atomically $ getWeapon actor
  case weaponMaybe of
    Just weapon -> do
      skill <- atomically $ getEffectiveSkillRank actor (itemWeaponType weapon)
      attackRoll <- rollDice 3
      -- if attackRoll < 5 || attackRoll <= skill
      return ()
    Nothing -> return ()
