module Character
(
  Character
) where

import Item

data Character =
    Character {
      charName :: String,
      leftHand :: Item,
      rightHand :: Item
    }

instance Tangible Character where
  
