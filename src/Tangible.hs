module Tangible
(
  Tangible(..)
) where

import Control.Concurrent.STM

import Types

class Tangible t where
  getLocation :: t -> STM Location
  getContainer :: t -> STM Container
  move :: t -> Container -> STM ()
  matchesDesc :: t -> [String] -> String -> STM Bool
  viewShortDesc :: t -> Character -> STM String
  viewLongDesc :: t -> Character -> STM String
