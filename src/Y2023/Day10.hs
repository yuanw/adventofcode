module Y2023.Day10 where

import Control.Monad
import Control.Monad.IO.Class

-- import Control.Monad.State
import Control.Monad.Trans.State
import Data.List (groupBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, maybe)

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
readGraph grid = M.fromList . filter (\(_, ps) -> not (null ps)) . map (\(p, ps) -> (p, filter (\(x, y) -> 0 <= x && x < row && 0 <= y && y < col) ps)) $ [adjList ((grid !! i) !! j) (i, j) | i <- [0 .. row - 1], j <- [0 .. col - 1]]
  where
    col = length $ head grid
    row = length grid

    adjList :: Char -> Point -> (Point, [Point])
    adjList 'S' (x, y) = ((x, y), [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])
    adjList '|' (x, y) = ((x, y), [(x - 1, y), (x + 1, y)])
    adjList '-' (x, y) = ((x, y), [(x, y - 1), (x, y + 1)])
    adjList 'L' (x, y) = ((x, y), [(x - 1, y), (x, y + 1)])
    adjList 'J' (x, y) = ((x, y), [(x - 1, y), (x, y - 1)])
    adjList '7' (x, y) = ((x, y), [(x, y - 1), (x + 1, y)])
    adjList 'F' (x, y) = ((x, y), [(x, y + 1), (x + 1, y)])
    adjList _ p = (p, [])

startPoint :: Grid -> Point
startPoint grid = head [(i, j) | i <- [0 .. row - 1], j <- [0 .. col - 1], ((grid !! i) !! j) == 'S']
  where
    col = length $ head grid
    row = length grid

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
data MazeState = MazeState Grid (Map Point Int) Graph Int
startMazeState :: Grid -> Point -> MazeState
startMazeState grid s = MazeState grid (M.singleton s 0) (readGraph grid) 1

bfs :: [Point] -> StateT MazeState IO (Map Point Int, Int)
bfs [] = do
    MazeState g m _ rd <- get
    return (m, rd)
bfs frontiers = do
    MazeState grid lvlMap g rd <- get
    -- _ <- liftIO $ print ("lvl " <> (show rd))
    let nextNodes = filter (`M.member` g) $ frontiers >>= (\p -> fromMaybe [] $ M.lookup p g)
        (newLvlMap, newFrontiers) = foldl (\(m, nf) p -> if M.member p m then (m, nf) else (M.insert p rd m, p : nf)) (lvlMap, []) nextNodes :: (Map Point Int, [Point])

    -- _ <- liftIO $ print ("frontiers  " <> show frontiers)
    -- _ <- liftIO $ print ("nextNodes " <> show nextNodes)
    -- _ <- liftIO $ print ("newFrontiers " <> show newFrontiers)
    _ <- liftIO $ drawGrid (fillGrid grid newLvlMap)
    put (MazeState grid newLvlMap g (rd + 1))
    bfs newFrontiers

fillGrid :: Grid -> Map Point Int -> [[String]]
fillGrid grid lvlMap = map (map snd) sorted
  where
    sorted = map (sortOn (snd . fst)) sortedByRow :: [[(Point, String)]]
    sortedByRow = groupBy (\a b -> (fst (fst a)) == (fst (fst b))) tupleList :: [[(Point, String)]]
    tupleList = [((i, j), maybe "." (show) (M.lookup (i, j) lvlMap)) | i <- [0 .. row - 1], j <- [0 .. col - 1]] :: [(Point, String)]
    col = length $ head grid
    row = length grid

getChar' :: Grid -> Point -> Char
getChar' grid (x, y) = (grid !! x) !! y

drawGrid :: (Show a) => [[a]] -> IO ()
drawGrid grid = forM_ grid (\row -> putStr (filter (\c -> c /= '\'' && c /= '"') $ concatMap show row) >> putStr "\n") >> putStr "\n"
testInput1 :: String
testInput1 =
    "\
    \.....\n\
    \.S-7.\n\
    \.|.|.\n\
    \.L-J.\n\
    \.....\n"

testInput2 :: String
testInput2 =
    "\
    \-L|F7\n\
    \7S-7|\n\
    \L|7||\n\
    \-L-J|\n\
    \L|-JF\n"

testInput3 :: String
testInput3 =
    "\
    \..F7.\n\
    \.FJ|.\n\
    \SJ.L7\n\
    \|F--J\n\
    \LJ...\n"

testInput4 :: String
testInput4 =
    "\
    \7-F7-\n\
    \.FJ|7\n\
    \SJLL7\n\
    \|F--J\n\
    \LJ.LJ\n"

partI :: IO ()
partI = do
    grid <- lines <$> return testInput2

    let s = startPoint grid
        state@(MazeState _ _ m' _) = startMazeState grid s

    -- print m'
    (m, l) <- evalStateT (bfs [s]) state
    let g' = fillGrid grid m
    print (maximum $ M.elems m)

-- -- print s
-- drawGrid g'
-- drawGrid (fillGrid grid (M.singleton s 0))

-- drawGrid g'
