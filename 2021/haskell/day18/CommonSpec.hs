module CommonSpec where

import Test.Hspec
import Text.Parsec
import Common

main :: IO ()
main = hspec $ do
  describe "p_lit" $ do
    it "parses a number" $ do
      parse p_lit "" "1231" `shouldBe` Right (Lit 1231)

  describe "p_pair" $ do
    it "parses a single pair" $ do
      parse p_pair "" "[1,2]" `shouldBe` Right (Pair (Lit 1) (Lit 2))

    it "parses a nested pair" $ do
      parse p_pair "" "[1,[2,3]]" `shouldBe` Right (Pair (Lit 1) (Pair (Lit 2) (Lit 3)))

    it "parses a doubly nested pair" $ do
      parse p_pair "" "[1,[2,[3,4]]]" `shouldBe` Right (Pair (Lit 1) (Pair (Lit 2) (Pair (Lit 3) (Lit 4))))

  describe "parseLine" $ do
    it "should correctly parse a line" $ do
      parseLine "[1,[2,[3,4]]]" `shouldBe` [("0", 1), ("10", 2), ("110", 3), ("111", 4)]

  describe "parseList" $ do
    it "should correctly parse a list" $ do
      parseList [("0", 1), ("10", 2), ("110", 3), ("111", 4)] `shouldBe` (Pair (Lit 1) (Pair (Lit 2) (Pair (Lit 3) (Lit 4))))

  describe "split" $ do
    it "should split a pair correctly" $ do
      (split $ parseLine "[0,13]") `shouldBe` parseLine "[0,[6,7]]"

    it "should leave a pair unchanged correctly" $ do
      (split $ parseLine "[0,9]") `shouldBe` parseLine "[0,9]"

  describe "isExplodingPair" $ do
    it "should correctly identify a pair" $ do
      isExplodingPair "00000" "00001" `shouldBe` True

    it "should not identify a pair without matching stems" $ do
      isExplodingPair "00000" "01001" `shouldBe` False

    it "should not identify a pair not nested deep enough" $ do
      isExplodingPair "000" "001" `shouldBe` False

    it "should not identify a pair at different tree levels" $ do
      isExplodingPair "000" "0001" `shouldBe` False

  describe "explode" $ do
    it "should explode correctly when no left" $ do
      (explode $ parseLine "[[[[[9,8],1],2],3],4]") `shouldBe` parseLine "[[[[0,9],2],3],4]"

    it "should explode correctly when no right" $ do
      (explode $ parseLine "[7,[6,[5,[4,[3,2]]]]]") `shouldBe` parseLine "[7,[6,[5,[7,0]]]]"

    it "should explode correctly to the right" $ do
      (explode $ parseLine "[[6,[5,[4,[3,2]]]],1]") `shouldBe` parseLine "[[6,[5,[7,0]]],3]"

    it "should explode correctly to the left" $ do
      (explode $ parseLine "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") `shouldBe` parseLine "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

  describe "reduce" $ do
    it "should reduce correctly" $ do
      (reduce $ parseLine "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") `shouldBe` parseLine "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

  describe "add" $ do
    it "should add the first line of the fourth example correctly" $ do
      (reduce $ add (parseLine "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]") (parseLine "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"))
        `shouldBe` parseLine "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"

  describe "reduceAll" $ do
    it "should correctly reduce the first example" $ do
      (reduceAll $ map parseLine ["[1,1]", "[2,2]", "[3,3]", "[4,4]"])
        `shouldBe` parseLine "[[[[1,1],[2,2]],[3,3]],[4,4]]"

    it "should correctly reduce the second example" $ do
      (reduceAll $ map parseLine ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"])
        `shouldBe` parseLine "[[[[3,0],[5,3]],[4,4]],[5,5]]"

    it "should correctly reduce the third example" $ do
      (reduceAll $ map parseLine ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"])
        `shouldBe` parseLine "[[[[5,0],[7,4]],[5,5]],[6,6]]"

    it "should correctly reduce the fourth example" $ do
      let example = ["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
                     "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
                     "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
                     "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
                     "[7,[5,[[3,8],[1,4]]]]",
                     "[[2,[2,2]],[8,[8,1]]]",
                     "[2,9]",
                     "[1,[[[9,3],9],[[9,0],[0,7]]]]",
                     "[[[5,[7,4]],7],1]",
                     "[[[[4,2],2],6],[8,7]]"]
      (reduceAll $ map parseLine example)
        `shouldBe` parseLine "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

  describe "magnitude" $ do
    it "should correctly calculate a simple example" $ do
      (magnitude $ parseList $ parseLine "[9,1]") `shouldBe` 29

    it "should correctly calculate a complex example" $ do
      (magnitude $ parseList $ parseLine "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") `shouldBe` 3488