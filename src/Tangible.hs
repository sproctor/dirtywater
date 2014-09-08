module Tangible
(
  Tangible(..),
  viewShortDescs
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

viewShortDescs :: (Tangible t) => [t] -> Character -> STM [String]
viewShortDescs [] _ = return []
viewShortDescs (x:xs) c = do
  rest <- viewShortDescs xs c
  desc <- viewShortDesc x c
  return $ desc:rest
