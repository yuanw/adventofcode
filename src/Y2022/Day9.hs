module Y2022.Day9 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (
    Parser,
    digit,
    endOfLine,
    parseOnly,
    string,
 )

import Data.Text qualified as T

import Data.Set qualified as Set

import Data.Either (fromRight)

type Point = (Int, Int)

-- head, tail
type RopeState = (Point, Point)

distant :: RopeState -> Int
distant ((hx, hy), (tx, ty)) = abs (hx - tx) `max` abs (hy - ty)

follow :: RopeState -> RopeState
follow s = if distant s < 2 then s else helper s
  where
    helper :: RopeState -> RopeState
    helper ((hx, hy), (tx, ty)) = case (compare hx tx, compare hy ty) of
        (EQ, GT) -> ((hx, hy), (tx, ty + 1))
        (EQ, LT) -> ((hx, hy), (tx, ty - 1))
        (LT, EQ) -> ((hx, hy), (tx - 1, ty))
        (GT, EQ) -> ((hx, hy), (tx + 1, ty))
        (GT, GT) -> ((hx, hy), (tx + 1, ty + 1))
        (LT, LT) -> ((hx, hy), (tx - 1, ty - 1))
        (GT, LT) -> ((hx, hy), (tx + 1, ty - 1))
        (LT, GT) -> ((hx, hy), (tx - 1, ty + 1))
        (_, _) -> error "incorrect comb"

data Movement = R Int | U Int | L Int | D Int deriving stock (Show)

parserMovement :: Parser Movement
parserMovement = (string "R " >> R . read <$> many digit) <|> (string "U " >> U . read <$> many digit) <|> (string "L " >> L . read <$> many digit) <|> (string "D " >> D . read <$> many digit)

movementParser :: Parser [Movement]
movementParser = many (parserMovement <* endOfLine)

move :: Movement -> RopeState -> [RopeState]
move (R 0) _ = []
move (R n) ((hx, hy), (tx, ty)) = let s' = follow ((hx, hy + 1), (tx, ty)) in s' : move (R (n - 1)) s'
move (U 0) _ = []
move (U n) ((hx, hy), (tx, ty)) = let s' = follow ((hx + 1, hy), (tx, ty)) in s' : move (U (n - 1)) s'
move (L 0) _ = []
move (L n) ((hx, hy), (tx, ty)) = let s' = follow ((hx, hy - 1), (tx, ty)) in s' : move (L (n - 1)) s'
move (D 0) _ = []
move (D n) ((hx, hy), (tx, ty)) = let s' = follow ((hx - 1, hy), (tx, ty)) in s' : move (D (n - 1)) s'

empty :: RopeState
empty = ((0, 0), (0, 0))

visited :: [Movement] -> (RopeState, [RopeState])
visited =
    foldl
        ( \(s, points) m ->
            let ns = move m s in (last ns, points ++ ns)
        )
        (empty, [empty])

-- ..##..
-- ...##.
-- .####.
-- ....#.
-- s###..
test :: IO ()
test = do
    input <- readFile "data/2022/day9.txt"
    let movements = fromRight [] $ parseOnly movementParser $ T.pack input
        -- (s, points) = visited [R 4 , U 4 , L 3, D 1, R 4, D 1, L 5, R 2]
        (_, points) = visited movements
    -- print movements
    -- print s
    -- print $ points

    print . Set.size . Set.fromList . map snd $ points

partI :: IO Int
partI = do
    input <- readFile "data/2022/day9.txt"
    let movements = fromRight [] $ parseOnly movementParser $ T.pack input
        -- (s, points) = visited [R 4 , U 4 , L 3, D 1, R 4, D 1, L 5, R 2]
        (_, points) = visited movements
    return . Set.size . Set.fromList . map snd $ points
