module Types where

import Control.Concurrent.STM

data NounOrd
  = NounAny
  | NounOrd Int
  deriving (Show, Eq)

newtype AdjectiveList = AdjectiveList [String] deriving (Show, Eq)

newtype Noun = Noun String deriving (Show, Eq)

newtype NounDesc = NounDesc (NounOrd, AdjectiveList, Noun) deriving (Show, Eq)

data Preposition
  = PrepOn
  | PrepIn
  | PrepBehind
  | PrepUnder
  | PrepAny
  deriving (Show, Eq)

data ObjectDesc
  = ObjectDesc (ObjectDesc, Preposition, NounDesc)
  | ObjectDescBase NounDesc
  deriving (Show, Eq)

data ExitDesc
  = ExitDirection Direction
  | ExitObject ObjectDesc

data Command
  = CmdNoArgs String
  | CmdObjectDesc (String, ObjectDesc)
  | CmdBadCommand
  deriving (Show, Eq)

data PlayerCommand
  = PlayerMove ExitDesc
  | PlayerLook
  | PlayerLookAt ObjectDesc
  | PlayerQuit
  | PlayerSay String
  | PlayerBadCmd

data Position = In | On deriving Eq

newtype Volume = Volume Int deriving (Ord, Eq, Show, Read)

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
    locationChars :: TVar [Character],
    locationItems :: TVar [Item]
  }

data Portal =
    ItemPortal { portalItem :: Item, portalDest :: Location }
  | DirectionPortal { directionDest :: Location, directionDir :: Direction }
