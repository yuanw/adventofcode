module Y2025.Day1Spec (spec) where

import Test.Hspec
import Y2025.Day1

spec :: Spec
spec = do
    describe "readTuple" $ do
        it "parses 'L68' correctly" $
            readTuple "L68" `shouldBe` ('L', 68)

        it "parses 'R48' correctly" $
            readTuple "R48" `shouldBe` ('R', 48)

        it "parses single digit numbers" $
            readTuple "L5" `shouldBe` ('L', 5)

        it "parses three digit numbers" $
            readTuple "R100" `shouldBe` ('R', 100)

    describe "accum" $ do
        it "handles single left rotation" $
            accum 50 [('L', 68)] `shouldBe` (-18, 0)
