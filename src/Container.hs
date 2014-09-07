module Container
(
) where

import Item
import Character

data Position =
    In
  | On
  | Under Containable
  | Behind Containable

class Container t where
  getContents :: t -> [(Position, Containable)]
  canAdd :: t -> Character -> Position -> Bool
  add :: t -> Containable -> Position -> STM ()
  canRemove :: t -> Character -> Containable -> Bool
  remove :: t -> Containable -> STM ()
