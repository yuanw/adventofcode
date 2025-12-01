module Y2025.Day1 where

import Control.Arrow (second)
import Data.List (uncons)
import Data.Maybe (fromJust)

readTuple :: String -> (Char, Int)
readTuple = second read . fromJust . uncons

shouldIncremPartI :: (Int, Int) -> Bool
shouldIncremPartI = (== 0) . snd

shouldIncremPartII :: (Int, Int) -> Bool
shouldIncremPartII (a, b) = (a /= b) || (b == 0)

accum' :: ((Int, Int) -> Bool) -> [(Char, Int)] -> (Int, Int)
accum' incrFnc = foldl foldFnc (50, 0)
  where
    foldFnc :: (Int, Int) -> (Char, Int) -> (Int, Int)
    foldFnc (acc, count) (dir, num) = (after, count')
      where
        before = if dir == 'L' then acc - num else acc + num
        after = before `mod` 100
        count' = if incrFnc (before, after) then count + 1 else count

accum :: Int -> [(Char, Int)] -> (Int, Int)
accum start = foldl (\(acc, count) (d, n) -> let n' = if d == 'L' then acc - n else acc + n; b = (n' == 0 || n' `mod` 100 == 0) :: Bool in (n', count + if b then 1 else 0)) (start, 0)

partI :: IO ()
partI = do
    a <- map readTuple . lines <$> readFile "data/2025/day1/real.txt"
    print $ snd (accum' shouldIncremPartI a)

partII :: IO ()
partII = do
    a <- map readTuple . lines <$> readFile "data/2025/day1/test.txt"
    print $ snd (accum' shouldIncremPartII a)
