{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day2 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

data Color = Red | Blue | Green deriving (Show, Eq, Ord)

data Game = Game {id :: Int, bags :: Map.Map Color Int} deriving (Show)

data Config = Config {totalBlueCount :: Int, totalRedCount :: Int, totalGreenCount :: Int} deriving (Show)

data Cube = Cube Int Color deriving (Show)

foldCube :: [Cube] -> Map.Map Color Int
foldCube = foldr (\(Cube i c) m -> Map.insertWith (+) c i m) Map.empty

cubeParser :: Parser Cube
cubeParser = do
    char ' '
    i <- decimal
    char ' '
    color <- (string "blue" >> return Blue) <|> (string "red" >> return Red) <|> (string "green" >> return Green)
    return $ Cube i color

gameParser :: Parser Game
gameParser = do
    string "Game "
    i <- decimal
    char ':'
    cubes <- cubeParser `sepBy` (char ';' <|> char ',')
    return $ Game i (foldCube cubes)

gamesParser :: Parser [Game]
gamesParser = many $ gameParser <* endOfLine

partI :: IO ()
partI = do
    input <- TIO.readFile "data/2023/day2.txt"
    let games = fromRight [] (parseOnly gamesParser input)
    print games
