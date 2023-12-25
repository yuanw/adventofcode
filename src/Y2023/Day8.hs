{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day8 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)

-- import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

data Direction = L | R deriving (Show)

type Node = String
newtype Path = Path {getPath :: (Node, Node)} deriving (Show)

type Network = Map Node Path
type Directions = [Direction]
type Input = (Directions, Network)

data World = World {getCurrentNode :: Node, getDirections :: Directions, getNetwork :: Map Node Path, getStep :: Int} deriving (Show)

directionParser :: Parser Direction
directionParser = (char 'R' >> return R) <|> (char 'L' >> return L)

directionsParser :: Parser Directions
directionsParser = many directionParser

nodeParser :: Parser (Node, Path)
nodeParser = do
    node <- takeTill (== ' ')
    skipSpace
    char '='
    skipSpace
    char '('
    l <- takeTill (== ',')
    char ','
    skipSpace
    r <- takeTill (== ')')
    char ')'
    return (T.unpack node, Path (T.unpack l, T.unpack r))

nodesParser :: Parser [(Node, Path)]
nodesParser = many $ nodeParser <* endOfLine

inputParser :: Parser Input
inputParser = do
    ds <- directionsParser
    endOfLine
    endOfLine
    nodes <- nodesParser
    return (ds, M.fromList nodes)

reachEnd :: World -> Bool
reachEnd = (== "ZZZ") . getCurrentNode

start :: Input -> World
start (ds, network) = World "AAA" ds network 0

next :: World -> World
next (World c ds ns s) = World (f currentNode d) ds ns (s + 1)
  where
    d = ds !! (s `mod` length ds)
    currentNode = getPath $ fromJust (M.lookup c ns) :: (Node, Node)
    f :: (Node, Node) -> Direction -> Node
    f (l, _) L = l
    f (_, r) R = r

run :: Input -> World
run input = h (start input)
  where
    h :: World -> World
    h w = if (reachEnd w) then w else (h (next w))

partI :: IO ()
partI = do
    rawInput <- TIO.readFile "data/2023/day8.txt"
    let e = parseOnly inputParser rawInput
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print (getStep $ run input)
