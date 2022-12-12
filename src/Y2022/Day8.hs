module Y2022.Day8 where
import Data.Char (digitToInt)

type Grid = [[Int]]
type Point = (Int, Int)


get :: Point -> Grid -> Int
get (x,y) =  (!! y) .(!! x)

getWidth :: Grid -> Int
getWidth = length . (!! 0)

getLength :: Grid -> Int
getLength = length

isVisibleFromLeft :: Point -> Grid -> Bool
isVisibleFromLeft (0, _) _ = True
isVisibleFromLeft (x,y) grid =  (isVisibleFromLeft (x-1, y) grid) && (get (x-1, y) grid < get(x,y) grid)

isVisibleFromTop :: Point -> Grid -> Bool
isVisibleFromTop (0, _) _ = True
isVisibleFromTop (x,y) grid =  (isVisibleFromTop (x, y-1) grid) && (get (x, y-1) grid < get(x,y) grid)

isVisibleFromRight :: Point -> Grid -> Bool
isVisibleFromRight (x, _) grid = x == getWidth grid
isVisibleFromRight (x,y) grid =  (isVisibleFromTop (x+1, y) grid) && (get (x+1, y)   grid < get(x,y) grid)


isVisibleFromBottom :: Point -> Grid -> Bool
isVisibleFromBottom (_, y) grid = y == getLength grid
isVisibleFromBottom (x, y) grid =  (isVisibleFromBottom (x, y+1) grid) && (get (x, y +1)   grid < get(x,y) grid)

isVisible :: Point -> Grid -> Bool
isVisible point grid = isVisibleFromLeft point grid || isVisibleFromRight point grid || isVisibleFromTop point grid || isVisibleFromBottom point grid

countVisible :: Grid -> Int
countVisible grid =  length [(x, y) | x  <- [0.. (getWidth grid) - 1 ], y <- [0.. (getLength grid) - 1], isVisible (x,y) grid]

readInput :: FilePath -> IO Grid
readInput file = map (map digitToInt ) . lines  <$> readFile file

testGrid :: IO Grid
testGrid = readInput "data/2022/day8-test.txt"

test :: IO Int
test = countVisible <$> testGrid
