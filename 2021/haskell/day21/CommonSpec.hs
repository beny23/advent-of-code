module CommonSpec where

import Test.Hspec
import Common

main :: IO ()
main = hspec $ do
  describe "nextpos" $ do
    it "should add a position without wrapping" $ do
      nextpos 4 4 `shouldBe` 8

    it "should correctly wrap" $ do
      nextpos 6 5 `shouldBe` 1

    it "should correctly wrap multiple times" $ do
      nextpos 8 13 `shouldBe` 1
