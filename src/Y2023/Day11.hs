module Y2023.Day11 where

import Data.Foldable (forM_)
import Data.List (subsequences)

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

allCombinations :: [Point] -> [(Point, Point)]
-- allCombinations = map (\ps -> (head ps, ps !! 1))  . filter ( (== 2) . length)  . subsequences
allCombinations = map (\ps -> (head ps, ps !! 1)) . subsequencesOfSize 2
manhattanDis :: Point -> Point -> Int
manhattanDis (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
    let l = length xs
     in if (n > l)
            then []
            else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs') =
        let next = subsequencesBySize xs'
         in zipWith
                (++)
                ([] : next)
                (map (map (x :)) next ++ [[]])

partI :: IO ()
partI = do
    grid <- expand . lines <$> readFile "data/2023/day11.txt"
    -- print (findGalaxies grid)
    -- print (manhattanDis (6, 1) (11, 5))
    print (sum . map (uncurry manhattanDis) . allCombinations $ findGalaxies grid)
