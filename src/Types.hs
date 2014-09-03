module Types
(
  Direction(North, Northeast, East, Southeast, South, Southwest, West, Northwest)
) where

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
