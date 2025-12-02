module Y2025.Day2 where

import Control.Monad ((=<<))
import Data.List.Split (splitOn)

invalid :: Int -> Bool
invalid n = [str !! i | i <- [0 .. half - 1]] == [str !! j | j <- [half .. l]]
  where
    str = show n
    l = length str - 1
    half = length str `div` 2

accum :: (Int, Int) -> [Int]
accum (start, end) = [i | i <- [start .. end], invalid i]

parseRanges :: String -> [(Int, Int)]
parseRanges input =
    [ (read a, read b)
    | pair <- splitOn "," input
    , length pair > 0
    , let [a, b] = splitOn "-" pair
    ]

partI :: IO ()
partI = do
    a <- parseRanges <$> readFile "data/2025/day2/input.txt"
    print $ foldr (+) 0 $ (accum =<< a)
