module Y2022.Day8 where
import Data.Char (digitToInt)

type Grid = [[Int]]
type Point = (Int, Int)


get :: Point -> Grid -> Int
get (x,y) =  (!! y) .(!! x)


isVisibleFromLeft :: Point -> Grid -> Bool
isVisibleFromLeft (0, _) _ = True
isVisibleFromLeft (x,y) grid =  (isVisibleFromLeft (x-1, y) grid) || (get (x-1, y) grid >= get(x,y) grid)




isVisibleFromTop :: Point -> Grid -> Bool
isVisibleFromTop = undefined

isVisibleFromRight :: Point -> Grid -> Bool
isVisibleFromRight = undefined


isVisibleFromBottom :: Point -> Grid -> Bool
isVisibleFromBottom = undefined

readInput :: FilePath -> IO Grid
readInput file = map (map digitToInt ) . lines  <$> readFile file

testGrid :: IO Grid
testGrid = readInput "data/2022/day8-test.txt"
