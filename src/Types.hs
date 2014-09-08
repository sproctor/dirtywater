module Types where

import Control.Concurrent.STM

data Command = Look | Exit | Move Direction deriving (Show, Eq)

data Position = In | On deriving Eq

data Volume = Int deriving (Ord, Eq, Show, Read)

data Item =
    Item {
      itemContainer :: TVar Container,
      itemName :: String,
      itemAdjs :: [String],
      itemContainers :: [(Position, Volume)],
      itemContainedItems :: [(Position, Item)],
      itemContainedChars :: [(Position, Character)]
    } deriving Eq

data Character =
    Character {
      charContainer :: TVar Container,
      charName :: TVar String
      --leftHand :: Item,
      --rightHand :: Item
    } deriving Eq

data Container = ContainerLocation Location | ContainerItem Item

data Direction =
    North
  | Northeast
  | East
  | Southeast
  | South
  | Southwest
  | West
  | Northwest
  deriving (Show, Eq)

data Location =
  Location {
    locationTitle :: String,
    locationDesc :: String,
    locationPortals :: TVar [Portal],
    locationCharacters :: TVar [Character],
    locationItems :: TVar [Item]
  }

data Portal =
    ItemPortal { portalItem :: Item, portalDest :: Location }
  | DirectionPortal { directionDest :: Location, directionDir :: Direction }
