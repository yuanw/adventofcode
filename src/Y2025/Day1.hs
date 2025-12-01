module Y2025.Day1 where

import Control.Arrow (second)
import Data.List (uncons)
import Data.Maybe (fromJust)

readTuple :: String -> (Char, Int)
readTuple = second read . fromJust . uncons

accum :: Int -> [(Char, Int)] -> (Int, Int)
accum start = foldr (\(d, n) (acc, count) -> let n' = if d == 'L' then acc - n else acc + n; b = n' == 0 || n' `mod` 100 == 0 :: Bool in (n', count + if b then 1 else 0)) (start, 0)

partI :: IO ()
partI = do
    a <- map readTuple . lines <$> readFile "data/2025/day1/test.txt"
    print $
        accum
            50
            [ ('L', 68)
            , ('L', 30)
            , ('R', 48)
            , ('L', 5)
            -- ,('R', 60)
            ]
