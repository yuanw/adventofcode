module Y2022.Day8 where
import Data.Char (digitToInt)

type Grid = [[Int]]
type Point = (Int, Int)


get :: Point -> Grid -> Int
get (x,y) =  (!! x) .(!! y)

getWidth :: Grid -> Int
getWidth = length . (!! 0)

getLength :: Grid -> Int
getLength = length

isVisibleFromLeft :: Point -> Grid -> Bool
isVisibleFromLeft (0,_) _ = True
isVisibleFromLeft (x,y) grid = let val = get (x,y) grid in all (> val) [get (i, y) grid | i <- [ 0..x-1]]




isVisibleFromTop :: Point -> Grid -> Bool
isVisibleFromTop (_, 0) _ = True
isVisibleFromTop (x,y) grid = let val = get (x,y) grid in all (> val) [get (x, i) grid | i <- [ 0..y-1]]





isVisibleFromRight :: Point -> Grid -> Bool
isVisibleFromRight (x,y) grid =   x == getWidth grid - 1 || let val = get (x,y) grid in all (> val) [get (i, y) grid | i <- [ x+1..getWidth grid -1]]




isVisibleFromBottom :: Point -> Grid -> Bool
isVisibleFromBottom (x, y) grid =  y == getLength grid - 1 ||  let val = get (x,y) grid in all (> val) [get (x,i) grid | i <- [ y +1..getWidth grid - 1]]




isVisible :: Point -> Grid -> Bool
isVisible point grid = isVisibleFromLeft point grid || isVisibleFromRight point grid || isVisibleFromTop point grid || isVisibleFromBottom point grid

countVisible :: Grid -> Int
countVisible grid =  length [(x, y) | x  <- [0.. (getWidth grid) - 1 ],
                             y <- [0.. (getLength grid) - 1], isVisible (x,y) grid]

readInput :: FilePath -> IO Grid
readInput file = map (map digitToInt ) . lines  <$> readFile file

testGrid :: IO Grid
testGrid = readInput "data/2022/day8-test.txt"

test :: IO ()
test = do
  grid <- testGrid
  print  [isVisible (x, y) grid | x  <- [0.. (getWidth grid) - 1 ],
                                  y <- [0.. (getLength grid) - 1]]
