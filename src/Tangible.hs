module Tangible
(
) where

import Location

data Container = ContainerTangible Tangible | ContainerLocation Location

data Tangible =
    Tangible {
      tangibleName :: String,
      tangibleAdjs :: [String],
      tangibleSDesc :: String,
      tangibleLDesc :: String,
      tangibleParent :: Container,
    }
