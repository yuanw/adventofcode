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

        it "handles single right rotation" $
            accum 50 [('R', 50)] `shouldBe` (100, 1)

        it "counts when passing through 0" $
            accum 50 [('L', 50)] `shouldBe` (0, 1)

        it "counts when passing through multiples of 100" $
            accum 50 [('R', 150)] `shouldBe` (200, 2)

        it "processes rotations in correct order (left to right)" $
            accum 50 [('L', 68), ('L', 30)] `shouldBe` (-48, 0)

        it "counts correctly for test example" $
            accum 50 [('L', 68), ('L', 30), ('R', 48)] `shouldBe` (0, 1)

        it "handles full test sequence" $
            accum 50 [('L', 68), ('L', 30), ('R', 48), ('L', 5)] `shouldBe` (-5, 1)

        it "handles empty list" $
            accum 50 [] `shouldBe` (50, 0)

        it "counts multiple passes through special values" $
            accum 0 [('R', 100), ('R', 100)] `shouldBe` (200, 3)

        it "handles negative to positive transitions through zero" $
            accum (-10) [('R', 20)] `shouldBe` (10, 1)
