module Combat where

import Control.Concurrent.STM
import qualified Data.ByteString.UTF8 as UTF8

import Character
import Item
import Location
import Roll
import Tangible
import Types

attackCharacter :: Character -> Character -> IO ()
attackCharacter actor target = do
  weaponMaybe <- atomically $ getWeapon actor
  case weaponMaybe of
    Just weapon -> do
      attackerSkill <- atomically $ getEffectiveSkillRank actor (itemWeaponType weapon)
      defenderSkill <- atomically $ getEffectiveSkillRank target (SkillId "dodge")
      attackRoll <- rollDice 3
      defenseRoll <- rollDice 3
      let hit = attackRoll < 5 || attackRoll <= attackerSkill
      let dodge = defenseRoll < 5 || defenseRoll <= defenderSkill
      location <- atomically $ getLocation actor
      if hit
        then
          if dodge
            then do
              sendToCharacter actor (UTF8.fromString ("%s dodged [" ++ (show defenseRoll) ++ "] your attack!")) [charShortDescription target]
              sendToCharacter target (UTF8.fromString ("You dodged [" ++ (show defenseRoll) ++ "] an attack from %s!")) [charShortDescription actor]
              sendToRoomExcept location [actor, target] (UTF8.fromString "%s dodged an attack from %s.") [charShortDescription target, charShortDescription actor]
            else do
              sendToCharacter actor (UTF8.fromString ("You hit [" ++ (show attackRoll) ++ "] %s!")) [charShortDescription target]
              sendToCharacter target (UTF8.fromString ("%s hit [" ++ (show attackRoll) ++ "] you!")) [charShortDescription actor]
              sendToRoomExcept location [actor, target] (UTF8.fromString ("%s attacked %s and landed a hit [" ++ (show attackRoll) ++ "].")) [charShortDescription actor, charShortDescription target]
        else do
          sendToCharacter actor (UTF8.fromString ("You missed [" ++ (show attackRoll) ++ "] %s!")) [charShortDescription target]
          sendToCharacter target (UTF8.fromString ("%s missed [" ++ (show attackRoll) ++ "] you!")) [charShortDescription actor]
          sendToRoomExcept location [actor, target] (UTF8.fromString ("%s attacked %s and missed [" ++ (show attackRoll) ++ "].")) [charShortDescription actor, charShortDescription target]
    Nothing -> return ()
