module Location
(
  Direction(..),
  Location
) where

import Item
import Character

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
    locationPortals :: [Portal],
    locationCharacters :: [Character],
    locationItems :: [Item],
  }

data Portal =
    ItemPortal { portalItem :: Item, portalDest :: Location }
  | DirectionPortal { directionDest :: Location, directionDir :: Direction }
