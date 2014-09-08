module Item
(
  Item,
  canAdd
) where

import Control.Concurrent.STM
import Data.List

import Types
import Tangible

instance Tangible Item where
  getLocation self = do
    con <- readTVar (itemContainer self)
    case con of
      ContainerLocation l -> return l
      ContainerItem i -> getLocation i

  getContainer self = readTVar (itemContainer self)

  move self = writeTVar (itemContainer self)

  matchesDesc self adjs name =
    isPrefixOf name (itemName self) && findAdjs adjs (itemAdjs self) []
    
  viewShortDesc _ _ = ""
  viewLongDesc _ _ = ""

canAdd :: Item -> Character -> Position -> Bool
canAdd item _ pos =
  not $ null $ filter (\ (p, _) -> p == pos) (itemContainers item)

-- helper functions
findAdjs :: [String] -> [String] -> [String] -> Bool
findAdjs [] _ _ = True
findAdjs _ [] _ = False
findAdjs (prefix:toFind) (adj:toSearch) searched =
  if isPrefixOf prefix adj
    then findAdjs toFind (putTogether searched toSearch) []
    else findAdjs (prefix:toFind) toSearch (adj:searched)

putTogether :: [String] -> [String] -> [String]
putTogether [] xs = xs
putTogether (x:rest) xs = putTogether rest (x:xs)
