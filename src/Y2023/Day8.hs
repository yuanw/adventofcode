{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day8 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text as T
import Data.Text.IO qualified as TIO

data Direction = L | R deriving (Show)

type Node = String
newtype Path = Path {getPath :: (Node, Node)} deriving (Show)

type Network = Map Node Path
type Directions = [Direction]
type Input = (Directions, Network)

directionParser :: Parser Direction
directionParser = (char 'R' >> return R) <|> (char 'L' >> return L)

directionsParser :: Parser Directions
directionsParser = many $ directionParser <* endOfLine

nodeParser :: Parser (Node, Path)
nodeParser = do
    node <- takeTill (\x -> x /= ' ')
    skipSpace
    char '='
    skipSpace
    char '('
    l <- takeTill (\x -> x /= ' ')
    char ','
    skipSpace
    r <- takeTill (\x -> x /= ' ')
    char ')'
    return $ (unpack node, Path (unpack l, unpack r))

nodesParser :: Parser [(Node, Path)]
nodesParser = many $ nodeParser <* endOfLine

inputParser :: Parser Input
inputParser = do
    ds <- directionsParser
    skipSpace
    nodes <- nodesParser
    return $ (ds, M.fromList nodes)

partI :: IO ()
partI = do
    rawInput <- TIO.readFile "data/2023/day8-test.txt"
    let e = parseOnly directionsParser rawInput
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print input
