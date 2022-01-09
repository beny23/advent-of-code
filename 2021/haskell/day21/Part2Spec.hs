module Part2Spec where

import Test.Hspec
import Part2 (countWinner, checkGameover, dedupe)

main :: IO ()
main = hspec $ do
  describe "countWinner" $ do
    it "should correctly identify player 1 as the winner" $ do
      countWinner (((3, 12), (8, 7)), 12) `shouldBe` [12, 0]

    it "should correctly identify player 2 as the winner" $ do
      countWinner (((3, 12), (8, 27)), 12) `shouldBe` [0, 12]

  describe "checkGameover" $ do
    it "should correctly identify no winner" $ do
      checkGameover (((3, 12), (8, 7)), 12) `shouldBe` False

    it "should correctly identify if player 1 winner" $ do
      checkGameover (((3, 21), (8, 7)), 12) `shouldBe` True

    it "should correctly identify if player 2 winner" $ do
      checkGameover (((3, 19), (8, 25)), 12) `shouldBe` True

  describe "dedupe" $ do
    it "should leave no dupes alone" $ do
      dedupe [(((3, 12), (8, 7)), 1), (((3, 12), (8, 9)), 1)] `shouldBe` [(((3, 12), (8, 7)), 1), (((3, 12), (8, 9)), 1)]

    it "correctly dedupe" $ do
      dedupe [(((3, 12), (8, 7)), 1), (((3, 12), (8, 7)), 1)] `shouldBe` [(((3, 12), (8, 7)), 2)]