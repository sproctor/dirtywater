{-# LANGUAGE OverloadedStrings #-}

module LocationSpec (main, spec) where

import Control.Concurrent.STM
import Test.Hspec

import Location
import Types

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Location" $ do
    let portal = Portal (LocationId "0") (LocationId "1")
    objs <- runIO $ atomically $ newTVar [ObjectDirection North portal]
    let loc = Location
                (LocationId "0")
                (StaticVisibleProperty "test")
                (StaticVisibleProperty "test")
                objs
    result <- runIO $ atomically $ getPortalByDirection loc North
    it "returns the portal assiated with the given direction" $ do
      result `shouldBe` (Just portal)
