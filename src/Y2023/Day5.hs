{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day5 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text.IO qualified as TIO

type Seed = Int
type Seeds = [Seed]

data MapEntry = MapEntry Int Int Int deriving (Show)

data Input = Input Seeds [[MapEntry]] deriving (Show)

skipRestOfLine :: Parser ()
skipRestOfLine = skipWhile (not . isEndOfLine) >> endOfLine

seedsParser :: Parser Seeds
seedsParser = string "seeds: " *> decimal `sepBy` char ' '

mapEntryParser :: Parser MapEntry
mapEntryParser = do
    a <- decimal
    char ' '
    b <- decimal
    char ' '
    c <- decimal
    return $ MapEntry a b c

mapEntriesParser :: Parser [MapEntry]
mapEntriesParser = many $ mapEntryParser <* endOfLine

mapping :: Int -> MapEntry -> Maybe Int
mapping v (MapEntry dest source range) = if v >= source && v <= (source + range) then Just (v - source + dest) else Nothing

mappingCategory :: Int -> [MapEntry] -> Int
mappingCategory v entries = fromMaybe v $ foldl (\accum entry -> if isJust accum then accum else mapping v entry) Nothing entries

mappingCategories :: Int -> [[MapEntry]] -> Int
mappingCategories = foldl mappingCategory

solve :: Input -> [Int]
solve (Input seeds entries) = map (\s -> mappingCategories s entries) seeds

inputParser :: Parser Input
inputParser = do
    seeds <- seedsParser
    endOfLine
    skipRestOfLine
    string "seed-to-soil map:"
    skipRestOfLine
    entries <- mapEntriesParser
    skipRestOfLine
    string "soil-to-fertilizer map:"
    skipRestOfLine
    s2f <- mapEntriesParser
    skipRestOfLine
    string "fertilizer-to-water map:"
    skipRestOfLine
    f2w <- mapEntriesParser
    skipRestOfLine
    string "water-to-light map:"
    skipRestOfLine
    w2l <- mapEntriesParser
    skipRestOfLine
    string "light-to-temperature map:"
    skipRestOfLine
    l2t <- mapEntriesParser
    skipRestOfLine
    string "temperature-to-humidity map:"
    skipRestOfLine
    t2h <- mapEntriesParser
    skipRestOfLine
    string "humidity-to-location map:"
    skipRestOfLine
    h2l <- mapEntriesParser
    return $ Input seeds [entries, s2f, f2w, w2l, l2t, t2h, h2l]

partI :: IO ()
partI = do
    rawInput <- TIO.readFile "data/2023/day5.txt"
    -- TIO.putStrLn input
    let e = (parseOnly inputParser rawInput)
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        -- Right logs ->  printDetails logs
        Right input@(Input _ entries) -> print (solve input)
