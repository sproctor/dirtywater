module Item
(
) where

import Container

data Item =
    Item {
      itemName :: String,
      itemAdjs :: [String],
      itemContainers :: [(Position, Int)]
      itemContents :: [(Position, Containable)]
    }

instance Container Item where
  getContents = itemContents
  canAdd item _ pos =
    not $ null $ filter (\ (p, _) -> p == pos) (itemContainers item)
