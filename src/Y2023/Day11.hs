module Y2023.Day11 where

import Data.Foldable (forM_)

type Grid = [String]
type Point = (Int, Int)

rowsNoGalaxies :: Grid -> [Int]
rowsNoGalaxies grid = [i | i <- [0 .. (rows - 1)], '#' `notElem` (grid !! i)]
  where
    rows = length grid

colsNoGalaxies :: Grid -> [Int]
colsNoGalaxies grid = [i | i <- [0 .. (cols - 1)], '#' `notElem` [(grid !! j) !! i | j <- [0 .. (rows - 1)]]]
  where
    rows = length grid
    cols = length (head grid)

expand :: Grid -> Grid
expand grid = expandCol (expandRow grid)
  where
    expandCol :: Grid -> Grid
    expandCol = map expandRowByRow
    cols = length (head grid)
    expandRowByRow row = foldl (\r i -> take i r ++ "." ++ drop i r) row colNeedExpand
    colNeedExpand = zipWith (+) (colsNoGalaxies grid) [0 ..]

    expandRow :: Grid -> Grid
    expandRow g = foldl (\g' i -> take i g' ++ [replicate cols '.'] ++ drop i g') g rowsNeedExpand
    rowsNeedExpand = zipWith (+) (rowsNoGalaxies grid) [0 ..]

drawGrid :: (Show a) => [[a]] -> IO ()
drawGrid grid = forM_ grid (\row -> putStr (filter (\c -> c /= '\'' && c /= '"') $ concatMap show row) >> putStr "\n") >> putStr "\n"

getCharFromGrid :: Grid -> Point -> Char
getCharFromGrid grid (x, y) = (grid !! x) !! y

findGalaxies :: Grid -> [Point]
findGalaxies grid = [(i, j) | i <- [0 .. (rows - 1)], j <- [0 .. (cols - 1)], getCharFromGrid grid (i, j) == '#']
  where
    rows = length grid
    cols = length (head grid)

partI :: IO ()
partI = do
    grid <- lines <$> readFile "data/2023/day11-test.txt"
    print (findGalaxies grid)
