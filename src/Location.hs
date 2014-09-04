module Location
(
  Direction(North, Northeast, East, Southeast, South, Southwest, West, Northwest),
  Location
) where

import Tangible

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

data Location = Location { locationTitle :: String, locationDesc :: String, locationPortals :: [Portal] }

data Portal =
    TangiblePortal { portalTangible :: Tangible, portalDest :: Location }
  | DirectionPortal { directionDest :: Location, directionDir :: Direction }
