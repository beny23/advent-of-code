module CommonSpec where

import Test.Hspec
import Data.List
import Common

main :: IO ()
main = hspec $ do
  describe "parseLine" $ do
    it "should correctly parse a line" $ do
      parseLine "..##" `shouldBe` [0,0,1,1]

  describe "addBorder" $ do
    it "should correctly add a border" $ do
      addBorder 1 [[0,1],[1,0]] `shouldBe` [[0,0,0,0],[0,0,1,0],[0,1,0,0],[0,0,0,0]]

    it "should correctly add a border width 3" $ do
      addBorder 3 [[0,1],[1,0]] `shouldBe` [[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,1,0,0,0],[0,0,0,1,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0]]

  describe "bin2dec" $ do
    it "should convert a binary list to a number" $ do
      bin2dec [0,0,0,1,0,0,0,1,0] `shouldBe` 34