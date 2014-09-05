module Tangible
(
  Tangible(getLocation, getContainer, move, matchesDesc, viewShortDesc, viewLongDesc)
) where

import Container
import Character
import Item

class TangibleClass where
  getLocation :: (Tangible a) => a -> Location
  getContainer :: (Tangible a, Container b) => a -> b
  move :: (Tangible a, Container b) => a -> b -> STM ()
  -- matchesDesc :: (Tangible a) => a -> [String] -> String -> Bool
  -- viewShortDesc :: (Tangible a) => a -> Character -> String
  -- viewLongDesc :: (Tangible a) => a -> Character -> String

data Tangible = TangibleItem Item | TangibleCharacter Character

instance TangibleClass Tangible where
  getLocation (TangibleItem i) = getLocation i
  getLocation (TangibleCharacter c) = getLocation c
  getContainer (TangibleItem i) = getContainer i
  getContainer (TangibleCharacter c) = getContainer c
  move (TangibleItem i) con = move i con
  move (TangibleCharacter c) con = move c con
  -- matchesDesc (Tangible)
