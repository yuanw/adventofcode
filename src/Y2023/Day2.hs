{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day2 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Map qualified as Map

data Color = Red | Blue | Green deriving (Show)

data Game = Game {id :: Int, bags :: Map.Map Color Int} deriving (Show)

data Config = Config {totalBlueCount :: Int, totalRedCount :: Int, totalGreenCount :: Int} deriving (Show)

data Cube = Cube Int Color deriving (Show)

foldCube :: [Cube] -> Map.Map Color Int
foldCube = (\(Cube i c) m -> Map.insert c i m) Map.empty

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
    _ <- cubeParser `sepBy` char ';'
    return $ Game i Map.empty
