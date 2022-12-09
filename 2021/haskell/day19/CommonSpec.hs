module CommonSpec where

import Test.Hspec
import Text.Parsec
import Data.List
import Common

main :: IO ()
main = hspec $ do
  describe "p_num" $ do
    it "should correctly parse positive numbers" $ do
      parse p_num "" "123" `shouldBe` Right 123

    it "should correctly parse negative numbers" $ do
      parse p_num "" "-13" `shouldBe` Right (-13)

  describe "p_nums" $ do
    it "should correctly parse comma separated numbers" $ do
      parse p_nums "" "123,-12" `shouldBe` Right ([123,-12])

    it "should correctly parse comma optional newlines" $ do
      parse p_nums "" "123,-12\n" `shouldBe` Right ([123,-12])

  describe "p_title" $ do
    it "should correctly parse the scanner header" $ do
      parse p_title "" "--- scanner 0 ---" `shouldBe` Right 0

  describe "p_scanner" $ do
    it "should correctly parse a scanner input" $ do
      parse p_scanner "" "--- scanner 0 ---\n1,2,3\n-1,-2,-3\n"
        `shouldBe` Right [[1,2,3], [-1,-2,-3]]

  describe "p_scanners" $ do
    it "should correctly parse scanners" $ do
      parse p_scanners "" "--- scanner 0 ---\n1,2,3\n-1,-2,-3\n\n--- scanner 1 ---\n4,5,6"
        `shouldBe` Right [[[1,2,3], [-1,-2,-3]], [[4,5,6]]]

  describe "normaliseFor" $ do
    it "should centre on the smallest input" $ do
      normaliseFor 0 [[1,1,1], [2,2,2], [3,3,3]] `shouldBe` [[0,0,0], [1,1,1], [2,2,2]]

    it "should centre with offset" $ do
      normaliseFor 1 [[1,1,1], [2,2,2], [3,3,3]] `shouldBe` [[-1,-1,-1], [0,0,0], [1,1,1]]

    it "should centre with offset to the end" $ do
      normaliseFor 2 [[1,1,1], [2,2,2], [3,3,3]] `shouldBe` [[-2,-2,-2], [-1,-1,-1], [0,0,0]]

  describe "rotateX" $ do
    it "should rotate around the X axis" $ do
      rotateX [1,2,3] `shouldBe` [1,-3,2]

  describe "rotateY" $ do
    it "should rotate around the Y axis" $ do
      rotateY [1,2,3] `shouldBe` [-3,2,1]

  describe "rotateZ" $ do
    it "should rotate around the Z axis" $ do
      rotateZ [1,2,3] `shouldBe` [2,1,-3]

  describe "rotate" $ do
    it "should rotate to 24 unique different directions" $ do
      (length $ nub $ rotate [1,2,3]) `shouldBe` 24