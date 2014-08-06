module ControllerSpec (main, spec) where

import Test.Hspec
--import Controller

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "unless test" $ do
    it "tests something that's not even in the module" $ do
      (1+1) `shouldBe` (2 :: Int)
