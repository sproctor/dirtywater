{-# LANGUAGE OverloadedStrings #-}

module ParseCommandSpec (main, spec) where

import Test.Hspec
import ParseCommand
import Types

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tests" $ do
    it "Parses a simple string" $ do
      parseCommand [] "" `shouldBe` Right (BadCommand "error")
