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

    describe "countCrossings" $ do
        it "counts one crossing going right from 50 to 100" $
            countCrossings 50 50 `shouldBe` 1

        it "counts zero crossings for small right rotation" $
            countCrossings 50 10 `shouldBe` 0

        it "counts two crossings for large right rotation" $
            countCrossings 50 150 `shouldBe` 2

        it "counts one crossing going left from 50 by 68" $
            countCrossings 50 (-68) `shouldBe` 1

        it "counts zero crossings for zero distance" $
            countCrossings 50 0 `shouldBe` 0

        it "counts crossings when starting at 0" $
            countCrossings 0 50 `shouldBe` 0

        it "counts crossing through 0 going right from 95" $
            countCrossings 95 10 `shouldBe` 1

    describe "processRotations with countPartII" $ do
        it "handles the example sequence correctly" $
            let rotations = [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82]
             in snd (processRotations countPartII rotations) `shouldBe` 6
