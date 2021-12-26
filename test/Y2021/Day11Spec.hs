
module Y2021.Day11Spec where
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec
import Y2021.Day11 (Grid, Position, step, readGrid, incr, flash, neighbors, solveDay11PartI)
import Y2021.Day10 (solveDay10PartI)

day0 :: String
day0 = unlines ["11111", "19991", "19191", "19991", "11111"]

day1 :: String
day1 = unlines ["34543","40004","50005","40004","34543"]

incrDay0 :: Grid
incrDay0 = M.fromList [((0,0),2),((0,1),2),((0,2),2),((0,3),2),((0,4),2),
                       ((1,0),2),((1,1),0),((1,2),0),((1,3),0),((1,4),2),
                       ((2,0),2),((2,1),0),((2,2),2),((2,3),0),((2,4),2),
                       ((3,0),2),((3,1),0),((3,2),0),((3,3),0),((3,4),2),
                       ((4,0),2),((4,1),2),((4,2),2),((4,3),2),((4,4),2)]
flash1 :: Grid
flash1 = M.fromList [((0,0),3),((0,1),3),((0,2),3),((0,3),2),((0,4),2),
                     ((1,0),3),((1,1),0),((1,2),0),((1,3),0),((1,4),2),
                     ((2,0),3),((2,1),0),((2,2),3),((2,3),0),((2,4),2),
                     ((3,0),2),((3,1),0),((3,2),0),((3,3),0),((3,4),2),
                     ((4,0),2),((4,1),2),((4,2),2),((4,3),2),((4,4),2)]

day2 :: String
day2 = unlines ["45654" ,"51115","61116","51115","45654"]

testGridStr :: String
testGridStr = unlines ["5483143223",
                       "2745854711",
                       "5264556173",
                       "6141336146",
                       "6357385478",
                       "4167524645",
                       "2176841721",
                       "6882881134",
                       "4846848554",
                       "5283751526"]

testGrid :: Grid
testGrid = readGrid testGridStr

spec :: Spec
spec = do
  describe "day 11" $ do
    it "readGrid" $ do
      readGrid day0 `shouldBe` M.fromList [((0,0),1),((0,1),1),((0,2),1),((0,3),1),((0,4),1),
                                           ((1,0),1),((1,1),9),((1,2),9),((1,3),9),((1,4),1),
                                           ((2,0),1),((2,1),9),((2,2),1),((2,3),9),((2,4),1),
                                           ((3,0),1),((3,1),9),((3,2),9),((3,3),9),((3,4),1),
                                           ((4,0),1),((4,1),1),((4,2),1),((4,3),1),((4,4),1)]

    it "step" $ do
      step (readGrid day0)  `shouldBe`  (readGrid day1, 9)
      fst (step (readGrid day1))  `shouldBe`  readGrid day2

    it "incr" $ do
      incr (readGrid day0)  `shouldBe`  (incrDay0, S.fromList [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (3,3)] )

    it "flash" $ do
      flash incrDay0 (1,1) `shouldBe` (flash1, S.empty)

    it "neighbors" $ do
      neighbors incrDay0 (1,1) `shouldBe` [(0,0), (0,1), (0,2),
                                           (1,0), (1,2),
                                           (2,0), (2,1), (2,2)]

    it "solve part I" $ do
      solveDay11PartI testGrid 11 `shouldBe` 204
