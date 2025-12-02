module Y2025.Day1 where

import Control.Arrow (second)
import Data.List (uncons)
import Data.Maybe (fromJust)

readTuple :: String -> (Char, Int)
readTuple = second read . fromJust . uncons

toNumber :: (Char, Int) -> Int
toNumber ('L', n) = negate n
toNumber (_, n) = n

incremPartI :: (Int, Int) -> Int
incremPartI (a, b) = if (a + b) `mod` 100 == 0 then 1 else 0

shouldIncremPartII :: (Int, Int) -> Int
shouldIncremPartII (a, b) = c + d + e
  where
    c = (abs b) `div` 100
    d = if b < 0 && a /= 0 && (abs b) >= a then 1 else 0
    e = if b > 0 && a + b >= 100 then 1 else 0

accum' :: ((Int, Int) -> Int) -> [Int] -> (Int, Int)
accum' incrFnc = foldl foldFnc (50, 0)
  where
    foldFnc :: (Int, Int) -> Int -> (Int, Int)
    foldFnc (acc, count) num = (after, count')
      where
        before = acc + num
        after = before `mod` 100
        count' = count + incrFnc (acc, num)

accum :: Int -> [(Char, Int)] -> (Int, Int)
accum start = foldl (\(acc, count) (d, n) -> let n' = if d == 'L' then acc - n else acc + n; b = (n' == 0 || n' `mod` 100 == 0) :: Bool in (n', count + if b then 1 else 0)) (start, 0)

partI :: IO ()
partI = do
    a <- map (toNumber . readTuple) . lines <$> readFile "data/2025/day1/real.txt"
    print $ snd (accum' incremPartI a)

partII :: IO ()
partII = do
    a <- map (toNumber . readTuple) . lines <$> readFile "data/2025/day1/real.txt"
    -- print $  (accum' shouldIncremPartII [-68, -30, 48])
    print $ snd (accum' shouldIncremPartII a)
