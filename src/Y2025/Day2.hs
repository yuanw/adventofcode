module Y2025.Day2 where

import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

-- Check if a number is invalid (digit sequence repeats exactly twice)
-- Keep for testing/validation
invalid :: Int -> Bool
invalid n = even (length str) && left == right
  where
    str = show n
    half = length str `div` 2
    (left, right) = splitAt half str

-- Generate invalid numbers for a specific even length within range
generateInvalidsForLength :: Int -> Int -> Int -> [Int]
generateInvalidsForLength start end totalLen
    | odd totalLen = [] -- Only even lengths are possible
    | otherwise = filter inRange $ map (toInvalid halfLen) patterns
  where
    halfLen = totalLen `div` 2
    minPattern = 10 ^ (halfLen - 1) -- First pattern without leading zero
    maxPattern = 10 ^ halfLen - 1 -- Last pattern for this length

    -- Optimize: only generate patterns that could be in range
    -- For pattern p, invalid number is: p * 10^halfLen + p
    firstPattern = max minPattern (start `div` (10 ^ halfLen + 1))
    lastPattern = min maxPattern ((end `div` (10 ^ halfLen + 1)) + 1)

    patterns = [firstPattern .. lastPattern]
    toInvalid h p = p * 10 ^ h + p
    inRange n = n >= start && n <= end

-- Main efficient accumulation function
accumEfficient :: (Int, Int) -> [Int]
accumEfficient (start, end) =
    concatMap (generateInvalidsForLength start end) candidateLengths
  where
    minLen = length (show start)
    maxLen = length (show end)
    -- Only even lengths, plus potential boundary lengths
    candidateLengths = Set.toList $ Set.fromList $ filter even [minLen .. maxLen + 1]

-- Original brute force (keep for testing small ranges)
accum :: (Int, Int) -> [Int]
accum (start, end) = filter invalid [start .. end]

-- Robust parsing with error handling
parseRange :: String -> Maybe (Int, Int)
parseRange s = case splitOn "-" s of
    [a, b] | not (null a) && not (null b) -> Just (read a, read b)
    _ -> Nothing

parseRanges :: String -> [(Int, Int)]
parseRanges = mapMaybe parseRange . filter (not . null) . splitOn ","

partI :: IO ()
partI = do
    ranges <- parseRanges <$> readFile "data/2025/day2/input.txt"
    print $ sum $ concatMap accumEfficient ranges

-- Part II: Repeating patterns (at least twice)

-- Helper: Find all proper divisors of n (divisors less than n)
properDivisors :: Int -> [Int]
properDivisors n = [d | d <- [1 .. n - 1], n `mod` d == 0]

-- Check if a number is invalid (digit sequence repeats at least twice)
invalidPartII :: Int -> Bool
invalidPartII n = any isRepeatingPattern (properDivisors len)
  where
    str = show n
    len = length str
    isRepeatingPattern patLen =
        let patter = take patLen str
            chunks = chunksOf patLen str
         in all (== patter) chunks

-- Generate invalid numbers for Part II within range
generateInvalidsPartII :: (Int, Int) -> [Int]
generateInvalidsPartII (start, end) =
    Set.toList $ Set.fromList $ concatMap genForLength [minLen .. maxLen]
  where
    minLen = length (show start)
    maxLen = length (show end)

    genForLength len =
        concatMap (genForLengthAndDiv len) (properDivisors len)

    genForLengthAndDiv totalLen patternLen =
        let repetitions = totalLen `div` patternLen
            minPattern = 10 ^ (patternLen - 1)
            maxPattern = 10 ^ patternLen - 1
            makeNum p = read (concat (replicate repetitions (show p)))
         in filter
                (\n -> n >= start && n <= end)
                [makeNum p | p <- [minPattern .. maxPattern]]

partII :: IO ()
partII = do
    ranges <- parseRanges <$> readFile "data/2025/day2/input.txt"
    print $ sum $ concatMap generateInvalidsPartII ranges
