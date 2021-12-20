module CommonSpec where

import Test.Hspec
import Common

main :: IO ()
main = hspec $ do
  describe "isHit" $ do
    it "registers a hit" $ do
      isHit (2, 2) ((0,0), (5, 5)) `shouldBe` True

    it "registers a miss above" $ do
      isHit (2, -2) ((0,0), (5, 5)) `shouldBe` False

    it "registers a miss below" $ do
      isHit (2, 7) ((0,0), (5, 5)) `shouldBe` False

    it "registers a miss left" $ do
      isHit (-2, 2) ((0,0), (5, 5)) `shouldBe` False

    it "registers a miss right" $ do
      isHit (7, 2) ((0,0), (5, 5)) `shouldBe` False

    it "registers a hit on the edge" $ do
      isHit (0, 2) ((0,0), (5, 5)) `shouldBe` True

  describe "canHit" $ do
    it "registers a hit with example 1" $ do
      canHit ((20, -10), (30, -5)) (0, 0) (7, 2) `shouldBe` True

    it "registers a hit with example 2" $ do
      canHit ((20, -10), (30, -5)) (0, 0) (6, 3) `shouldBe` True

    it "registers a hit with example 3" $ do
      canHit ((20, -10), (30, -5)) (0, 0) (9, 0) `shouldBe` True

    it "does not register a hit with example 4" $ do
      canHit ((20, -10), (30, -5)) (0, 0) (17, -4) `shouldBe` False

    it "registers a hit with example 5" $ do
      canHit ((20, -10), (30, -5)) (0, 0) (6, 9) `shouldBe` True

  describe "helpers" $ do
    it "maxX is correct" $ do
      maxX ((20, -10), (30, -5)) `shouldBe` 30

    it "minY is correct" $ do
      minY ((20, -10), (30, -5)) `shouldBe` -10

  describe "findHits" $ do
    it "finds the correct example" $ do
      findHits ((20, -10), (30, -5)) (0, 0) `shouldContain` [(6, 9)]

  describe "findHighest" $ do
    it "finds the highest point" $ do
      findHighest (findHits ((20, -10), (30, -5)) (0, 0)) `shouldBe` 45