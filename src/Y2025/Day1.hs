module Y2025.Day1 where

import Control.Arrow (second)
import Data.List (uncons)
import Data.Maybe (fromJust)

-- Input parsing
readTuple :: String -> (Char, Int)
readTuple = second read . fromJust . uncons

toNumber :: (Char, Int) -> Int
toNumber ('L', n) = negate n
toNumber (_, n) = n

-- Core counting logic: count crossings through multiples of 100
-- Counts multiples in (pos, pos+dist] for positive dist, or [pos+dist, pos) for negative dist
-- That is: exclusive of start position, inclusive of end position
countCrossings :: Int -> Int -> Int
countCrossings pos dist
    | dist > 0 = (pos + dist) `div` 100 - pos `div` 100
    | dist < 0 = (pos - 1) `div` 100 - (pos + dist - 1) `div` 100
    | otherwise = 0

-- Part I: Only count if final position is multiple of 100
countPartI :: Int -> Int -> Int
countPartI pos dist = if (pos + dist) `mod` 100 == 0 then 1 else 0

-- Part II: Count all crossings during rotation
countPartII :: Int -> Int -> Int
countPartII = countCrossings

-- Unified processing function
processRotations :: (Int -> Int -> Int) -> [Int] -> (Int, Int)
processRotations countFn = foldl step (50, 0)
  where
    step (pos, count) dist = (newPos, count + countFn pos dist)
      where
        newPos = ((pos + dist) `mod` 100 + 100) `mod` 100

partI :: IO ()
partI = do
    rotations <- map (toNumber . readTuple) . lines <$> readFile "data/2025/day1/real.txt"
    print $ snd $ processRotations countPartI rotations

partII :: IO ()
partII = do
    rotations <- map (toNumber . readTuple) . lines <$> readFile "data/2025/day1/real.txt"
    print $ snd $ processRotations countPartII rotations
