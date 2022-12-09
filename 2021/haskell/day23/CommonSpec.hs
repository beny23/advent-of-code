module CommonSpec where

import Test.Hspec
import Common

main :: IO ()
main = hspec $ do
  describe "enter" $ do
    it "should allow a single pod to enter" $ do
      enter 0 Amber [Cave Amber [Empty, Empty]] [Empty] `shouldBe` [(3, [Empty, Cave Amber [Empty, Pod Amber]])]

    it "should allow a single pod to enter a partially occupied cave" $ do
      enter 0 Amber [Cave Amber [Empty, Pod Amber]] [Empty] `shouldBe` [(2, [Empty, Cave Amber [Pod Amber, Pod Amber]])]

    it "should gracefully fail if no cave available" $ do
      enter 0 Amber [Cave Bronze [Empty, Empty]] [Empty] `shouldBe` []

    it "should not allow a pod to enter a cave that's not yet emptied" $ do
      enter 0 Amber [Cave Amber [Empty, Pod Bronze]] [Empty] `shouldBe` []

    it "should find the first cave correctly" $ do
      enter 0 Amber [Empty, Cave Amber [Empty, Empty], Empty, Cave Bronze [Empty, Empty]] [Empty]
        `shouldBe` [(4, [Empty, Empty, Cave Amber [Empty, Pod Amber], Empty, Cave Bronze [Empty, Empty]])]

    it "should find the second cave correctly" $ do
      enter 0 Amber [Empty, Cave Bronze [Empty, Empty], Empty, Cave Amber [Empty, Empty]] [Empty]
        `shouldBe` [(6, [Empty, Empty, Cave Bronze [Empty, Empty], Empty, Cave Amber [Empty, Pod Amber]])]

  describe "exit" $ do
    it "should exit to tile after" $ do
      exit 0 Amber [Empty] [Cave Amber [Empty, Empty]]
        `shouldBe` [(2, [Cave Amber [Empty, Empty], Pod Amber])]

    it "should exit to two tiles after" $ do
      exit 0 Amber [Empty, Empty] [Cave Amber [Empty, Empty]]
        `shouldBe` [(2, [Cave Amber [Empty, Empty], Pod Amber, Empty]),
                    (3, [Cave Amber [Empty, Empty], Empty, Pod Amber])]

  describe "complete" $ do
    it "should be complete with two filled caves" $ do
      complete [Empty, Empty, Cave Bronze [Pod Bronze, Pod Bronze], Empty, Cave Copper [Pod Copper, Pod Copper]]
        `shouldBe` True

    it "should not be complete with pods outside the cave" $ do
      complete [Empty, Empty, Cave Bronze [Pod Bronze, Pod Bronze], Empty, Cave Copper [Pod Copper, Pod Copper], Pod Amber]
        `shouldBe` False

  describe "move" $ do
    it "should move from outside into it's designated cave" $ do
      move 0 [Pod Amber, Empty, Cave Amber [Empty, Empty]] []
        `shouldBe` [(4, [Empty, Empty, Cave Amber [Empty, Pod Amber]])]

    it "should move reverse into it's designated cave" $ do
      move 0 [Empty, Cave Amber [Empty, Pod Amber], Pod Amber] []
        `shouldBe` [(2, [Empty, Cave Amber [Pod Amber, Pod Amber], Empty])]

    it "should move out of the back cave" $ do
      move 0 [Cave Bronze [Empty, Pod Amber], Empty] []
        `shouldBe` [(3, [Cave Bronze [Empty, Empty], Pod Amber])]

    it "should move out of the front cave" $ do
      move 0 [Cave Bronze [Pod Amber, Pod Bronze], Empty] []
        `shouldBe` [(2, [Cave Bronze [Empty, Pod Bronze], Pod Amber])]

    it "should move out both options" $ do
      move 0 [Cave Bronze [Empty, Pod Amber], Empty, Empty] []
        `shouldBe` [(3, [Cave Bronze [Empty, Empty], Pod Amber, Empty]), (4, [Cave Bronze [Empty, Empty], Empty, Pod Amber])]

    it "should move pod out of cave if other type still in there" $ do
      move 0 [Cave Bronze [Pod Bronze, Pod Amber], Empty] []
        `shouldBe` [(20, [Cave Bronze [Empty, Pod Amber], Pod Bronze])]

    it "should not move pod into cave if other type still in there" $ do
      move 0 [Pod Bronze, Cave Bronze [Empty, Pod Amber], Empty] []
        `shouldBe` [(3, [Pod Bronze, Cave Bronze [Empty, Empty], Pod Amber])]

    it "should enter and exit" $ do
      move 0 [Empty, Cave Copper [Empty, Pod Amber], Cave Bronze [Empty, Empty], Pod Bronze] []
        `shouldBe` [(3, [Pod Amber, Cave Copper [Empty, Empty], Cave Bronze [Empty, Empty], Pod Bronze]),
                    (30, [Empty, Cave Copper [Empty,Pod Amber], Cave Bronze [Empty, Pod Bronze], Empty])]
