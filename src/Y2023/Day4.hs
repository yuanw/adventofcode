{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day4 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as TIO

data Card = Card {cardId :: Int, winNo :: [Int], myNo :: [Int]} deriving (Show)

cardParser :: Parser Card
cardParser = do
    _ <- string "Card"
    skipSpace
    i <- decimal
    _ <- string ":"
    skipSpace
    wN <- many1 (decimal <* skipSpace)
    skipSpace
    _ <- string "|"
    skipSpace
    mN <- many1 (decimal <* skipSpace)
    return $ Card i wN mN

cardsParser :: Parser [Card]
cardsParser = many cardParser

partI :: IO ()
partI = do
    input <- TIO.readFile "data/2023/day4.txt"
    -- TIO.putStrLn input
    case (parseOnly cardsParser input) of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right card -> print card
