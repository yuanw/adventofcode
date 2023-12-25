module Y2023.Day8 where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO

data Direction = L | R

type Node = String
newtype Path = Path {getPath :: (Node, Node)}

type Network = Map Node Path
type Directions = [Direction]

partI :: IO ()
partI = do
    input <- TIO.readFile "data/2023/day8-test.txt"
