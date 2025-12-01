module Y2025.Day1 where

import Control.Arrow (second)
import Data.List (uncons)
import Data.Maybe (fromJust)

readTuple :: String -> (Char, Int)
readTuple = second read . fromJust . uncons

accum :: Int -> [(Char, Int)] -> (Int, Int)
accum start = foldl (\(acc, count) (d, n) -> let n' = if d == 'L' then acc - n else acc + n; b = (n' == 0 || n' `mod` 100 == 0) :: Bool in (n', count + if b then 1 else 0)) (start, 0)

partI :: IO ()
partI = do
    a <- map readTuple . lines <$> readFile "data/2025/day1/real.txt"
    print $ snd (accum 50 a)
