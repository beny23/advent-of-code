module CommonSpec where

import Test.Hspec
import Text.Parsec
import Common

main :: IO ()
main = hspec $ do
  describe "p_num" $ do
    it "should correctly parse positive numbers" $ do
      parse p_num "" "123" `shouldBe` Right 123

    it "should correctly parse negative numbers" $ do
      parse p_num "" "-13" `shouldBe` Right (-13)

  describe "p_range" $ do
    it "should correctly parse a range of numbers" $ do
      parse p_range "" "123..-12" `shouldBe` Right [123,-11]

  describe "p_cube" $ do
    it "should correctly parse a three ranges" $ do
      parse p_cube "" "x=1..2,y=-1..4,z=-5..5" `shouldBe` Right [[1,-1,-5], [3,5,6]]

  describe "p_instruction" $ do
    it "should correctly parse an on instruction" $ do
      parse p_instruction "" "on x=1..2,y=-1..4,z=-5..5" `shouldBe` Right ([[1,-1,-5], [3,5,6]], 1)

    it "should correctly parse an off instruction" $ do
      parse p_instruction "" "off x=-1..2,y=1..4,z=-5..5" `shouldBe` Right ([[-1,1,-5], [3,5,6]], -1)

  describe "parseLines" $ do
    it "should correctly parse lines" $ do
      parseLines ["on x=1..2,y=-1..4,z=-5..5", "off x=-1..2,y=1..4,z=-5..5"]
        `shouldBe` [([[1,-1,-5],[3,5,6]], 1), ([[-1,1,-5],[3,5,6]], -1)]

  describe "isInner" $ do
    it "should correctly identify inner cube" $ do
      isInner [[1,-1,-5],[3,5,6]] `shouldBe` True

    it "should correctly identify outer cube" $ do
      isInner [[1,-1,-5],[3,5,500]] `shouldBe` False

  describe "volume" $ do
    it "should correctly calculate the volume of the example cube" $ do
      volume [[10, 10, 10], [13, 13, 13]] `shouldBe` 27

    it "should correctly calculate the volume of an invalid cube" $ do
      volume [[13, 13, 13], [10, 10, 10]] `shouldBe` 0

    it "should correctly calculate the volume of a double negative cube" $ do
      volume [[-4,2,-1],[-3,-1,-3]] `shouldBe` 0

  describe "inter" $ do
    it "should correctly intersect" $ do
      inter [[10,10,10],[13,13,13]] [[11,11,11],[14,14,14]] `shouldBe` [[11,11,11],[13,13,13]]

    it "should correctly intersect on edges" $ do
      inter [[10,10,10],[13,13,13]] [[10,10,12],[13,13,13]] `shouldBe` [[10,10,12],[13,13,13]]

    it "should correctly deal with adjacent cubes" $ do
      (volume $ inter [[10,10,10],[13,13,13]] [[10,10,13],[13,13,16]]) `shouldBe` 0

  describe "mergeAll" $ do
    it "should correctly fold the first example instruction" $ do
      (sum $ map cuboids $ foldl mergeAll [] [([[10,10,10],[13,13,13]], 1)]) `shouldBe` 27

    it "should correctly fold the second example instruction" $ do
      (sum $ map cuboids $ foldl mergeAll [] [([[10,10,10],[13,13,13]], 1), ([[11,11,11],[14,14,14]], 1)]) `shouldBe` 46

    it "should correctly fold the third example instruction" $ do
      (sum $ map cuboids $ foldl mergeAll [] [([[10,10,10],[13,13,13]], 1), ([[11,11,11],[14,14,14]], 1), ([[9,9,9],[12,12,12]], -1)]) `shouldBe` 38

    it "should correctly fold the fourth example instruction" $ do
      (sum $ map cuboids $ foldl mergeAll [] [([[10,10,10],[13,13,13]], 1), ([[11,11,11],[14,14,14]], 1), ([[9,9,9],[12,12,12]], -1), ([[10,10,10],[11,11,11]], 1)]) `shouldBe` 39
