module Character
(
  Character
) where

import Item

data Character =
    Character {
      charContainer :: TVar Container,
      charName :: String,
      leftHand :: Item,
      rightHand :: Item
    }

instance Tangible Character where
  getLocation self = do
    con <- readTVar (charContainer self)
    case con of
      | ContainerLocation l = return l
      | ContainerItem i = getLocation i

  getContainer self = readTVar (charContainer self)

  move self = writeTVar (charContainer self)

  matchesDesc self [] name =
    isPrefixOf name (itemName self)
  matchesDesc _ _ _ = false
    
  viewShortDesc = ""
  viewLongDesc = ""

