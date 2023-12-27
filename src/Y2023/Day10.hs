module Y2023.Day10 where

import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

type Point = (Int, Int)
type Grid = [String]

type Graph = Map Point [Point]

{- | is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
-}
readGraph :: Grid -> Graph
readGraph grid = M.fromList . filter (\(_, ps) -> not (null ps)) . map (\(p, ps) -> (p, filter (\(x, y) -> 0 <= x && x < width && 0 <= y && y < height) ps)) $ [adjList ((grid !! j) !! i) (i, j) | i <- [0 .. width - 1], j <- [0 .. height - 1]]
  where
    width = length $ head grid
    height = length grid

    adjList :: Char -> Point -> (Point, [Point])
    adjList 'S' (x, y) = ((x, y), [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])
    adjList '|' (x, y) = ((x, y), [(x - 1, y), (x + 1, y)])
    adjList '-' (x, y) = ((x, y), [(x, y - 1), (x, y + 1)])
    adjList 'L' (x, y) = ((x, y), [(x - 1, y), (x, y + 1)])
    adjList 'J' (x, y) = ((x, y), [(x - 1, y), (x, y - 1)])
    adjList '7' (x, y) = ((x, y), [(x - 1, y), (x, y + 1)])
    adjList 'F' (x, y) = ((x, y), [(x + 1, y), (x, y + 1)])
    adjList _ p = (p, [])

startPoint :: Grid -> Point
startPoint grid = head [(i, j) | i <- [0 .. width - 1], j <- [0 .. height - 1], ((grid !! j) !! i) == 'S']
  where
    width = length $ head grid
    height = length grid

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame [] = do
    (_, score) <- get
    return score
playGame (x : xs) = do
    (on, score) <- get
    case x of
        'a' | on -> put (on, score + 1)
        'b' | on -> put (on, score - 1)
        'c' -> put (not on, score)
        _ -> put (on, score)
    playGame xs

startState = (False, 0)

-- level = {s: 0}
-- parent = {s: None}
-- frontiers = [s]
-- i = i
-- while frontiers:
--     next_nodes = []
--     for u in froniters:
--         for v in Adj[u]:
--             if v not in level:
--                 level[v] = i
--                 parent[v] = u
--                 next_nodes.append(v)
--     froniters = next_nodes
--     i += 1
data MazeState = MazeState (Map Point Int) [Point] Graph
startMazeState :: Grid -> MazeState
startMazeState grid = undefined

-- print $ evalState (playGame "abcaaacbbcabbab") startState
partI :: IO ()
partI = do
    a <- lines <$> readFile "data/2023/day10-2-test.txt"
    print (readGraph a)
