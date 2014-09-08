module Container
(
  Container(..)
) where

import Item
import Location

data Container = ContainerLocation Location | ContainerItem Item
