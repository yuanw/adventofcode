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
shouldIncremPartII (a, b)
    | b > 0 = (a + b) `div` 100 -- Right rotation: count passes through multiples of 100
    | b < 0 = let d = abs b in d `div` 100 + if a > 0 && a <= d `mod` 100 then 1 else 0 -- Left rotation
    | otherwise = 0

accum' :: ((Int, Int) -> Int) -> [Int] -> (Int, Int)
accum' incrFnc = foldl foldFnc (50, 0)
  where
    foldFnc :: (Int, Int) -> Int -> (Int, Int)
    foldFnc (acc, count) num = (after, count')
      where
        before = acc + num
        after = ((before `mod` 100) + 100) `mod` 100 -- Proper positive modulo
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
