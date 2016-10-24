module Tangible
(
  Tangible(..)
) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Types

class Tangible t where
  getLocation :: t -> STM Location
  getContainer :: t -> STM Container
  setContainer :: t -> Container -> STM ()
  matchesDesc :: t -> [ByteString] -> ByteString -> IO Bool
  viewShortDesc :: t -> Character -> IO ByteString
  viewLongDesc :: t -> Character -> IO ByteString
