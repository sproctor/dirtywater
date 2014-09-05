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

data Container = ContainerItem Item | ContainerLocation Location

class ContainerClass where
  getContents :: (Container a) => a -> [(Position, Containable)]
  canAdd :: (Container a) => a -> Character -> Position -> Bool
  add :: (Container a) => a -> Containable -> Position -> STM ()
  canRemove :: (Container a) => a -> Character -> Containable -> Bool
  remove :: (Container a) => a -> Containable -> STM ()

instance ContainerClass Container where
  getContents (ContainerItem i) = getContents i
  getContents (ContainerLocation l) = getContents l
  canAdd (ContainerItem i) c p = canAdd i c p
  canAdd (ContainerLocation l) c p = l c p
  add (ContainerItem i) t p = i t p
  add (ContainerLocation l) 
