module Y2023.Day3 where

import Data.Char (digitToInt, isDigit)

type Grid = [[Char]]
type Point = (Int, Int)

data State = State {currentNum :: [Point], allNum :: [[Point]]}

lookUp :: Point -> Grid -> Char
lookUp (x, y) g = (g !! y) !! x

appendChar :: Point -> Grid -> State -> State
appendChar p g (State cN aL) = if isDigit c then State (cN ++ [p]) aL else (State [] (if null cN then aL else cN : aL))
  where
    c = lookUp p g

partI :: IO ()
partI = do
    grid <- lines <$> readFile "data/2023/day3-test.txt"
    let y = length grid
        x = length (head grid)
    print (x, y)
