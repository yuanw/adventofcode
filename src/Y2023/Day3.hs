module Y2023.Day3 where

import Control.Monad (join)
import Data.Char (isDigit, isSymbol)
import Data.List (any, or, sortBy)

type Grid = [[Char]]
type Point = (Int, Int)

data State = State {currentNum :: [Point], allNum :: [[Point]]} deriving (Show)
zero :: State
zero = State [] []
mySort :: Point -> Point -> Ordering
mySort (x1, y1) (x2, y2) = case y1 `compare` y2 of
    EQ -> x1 `compare` x2
    other -> other

lookUp :: Point -> Grid -> Char
lookUp (x, y) g = (g !! y) !! x

appendChar :: Point -> Grid -> State -> State
appendChar p g (State cN aL) = if isDigit c then State (p : cN) aL else State [] (if null cN then aL else aL ++ [cN])
  where
    c = lookUp p g

lvlFold :: State -> Grid -> Int -> Int -> State
lvlFold s grid y width = cleanUp $ foldl (\s' p -> appendChar p grid s') s [(i, y) | i <- [0 .. width - 1]]
  where
    cleanUp :: State -> State
    cleanUp (State n allN) = State [] (if null n then allN else allN ++ [n])

gridFold :: State -> Grid -> Int -> Int -> State
gridFold s grid heigh width = foldl (\s' y -> lvlFold s' grid y width) s [0 .. heigh - 1]

getNumbers :: [[Point]] -> Grid -> [Int]
getNumbers ps grid = map helper ps
  where
    helper :: [Point] -> Int
    helper points = read $ foldl (\s point -> lookUp point grid : s) "" points

getNeighour :: Int -> Int -> Point -> [Point]
getNeighour heigh width (x, y) =
    filter (\(a, b) -> a > 0 && a < width - 1 && b > 0 && b < heigh - 1) $
        map
            (\(a, b) -> (a + x, b + y))
            [ (-1, -1)
            , (0, -1)
            , (1, -1)
            , (-1, 0)
            , (1, 0)
            , (-1, 1)
            , (0, 1)
            , (1, 1)
            ]

adjacentToSymbol :: Int -> Int -> Grid -> [Point] -> Bool
adjacentToSymbol heigh width grid points = or $ map ((\c -> not (isDigit c || c == '.')) . (flip lookUp) grid) $ join (map (\p -> getNeighour heigh width p) points)

test :: Int -> Int -> Grid -> [Point] -> String
test heigh width grid points = map (`lookUp` grid) $ join (map (getNeighour heigh width) points)

partI :: IO ()
partI = do
    grid <- lines <$> readFile "data/2023/day3.txt"
    let y = length grid
        x = length (head grid)

        state = gridFold zero grid y x
        pointOfNum = allNum state
        wantedPointOfNum = filter (adjacentToSymbol y x grid) pointOfNum
    -- print pointOfNum
    -- print (test y x grid [(2,0),(1,0),(0,0)])
    -- print (adjacentToSymbol   y x grid [(2,0),(1,0),(0,0)])
    print (sum $ getNumbers wantedPointOfNum grid)
