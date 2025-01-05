module Y2024.Day5 where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (Foldable (toList))
import Data.Graph.Inductive qualified as G
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Traversable
import Data.Void (Void)
import Linear.V2 (V2 (..))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as PL

type CharParser = P.Parsec Void String

type Input = ([(Int, Int)], [[Int]])

-- | 'sepEndBy' but automatically exclude the separator from the internal parser
sepEndBy' :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s sep -> P.Parsec e s [a]
sepEndBy' x sep = P.sepEndBy (P.notFollowedBy sep *> P.try x) sep

sepEndByLines :: (P.Stream s, Ord e, P.Token s ~ Char) => P.Parsec e s a -> P.Parsec e s [a]
sepEndByLines = flip sepEndBy' P.newline

pDecimal :: forall a e s. (P.Stream s, P.Token s ~ Char, Ord e, Num a) => P.Parsec e s a
pDecimal = pTok $ PL.signed P.space PL.decimal

pTok :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s a -> P.Parsec e s a
pTok p = pSpace *> p <* pSpace

pSpace :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s ()
pSpace = P.skipMany (P.char ' ')

sepByLines :: (P.Stream s, Ord e, P.Token s ~ Char) => P.Parsec e s a -> P.Parsec e s [a]
sepByLines = flip sepBy' P.newline

-- | 'sepBy' but automatically exclude the separator from the internal parser
sepBy' :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s sep -> P.Parsec e s [a]
sepBy' x sep = P.sepBy (P.notFollowedBy sep *> P.try x) sep

sequenceSepBy ::
    (Traversable t, P.Stream s, Ord e) => t (P.Parsec e s a) -> P.Parsec e s sep -> P.Parsec e s (t a)
sequenceSepBy xs sep = sequenceA . snd $ mapAccumR go False xs
  where
    go addSep x = (True, if addSep then x' <* sep else x')
      where
        x' = P.notFollowedBy sep *> P.try x

parseRule :: CharParser (Int, Int)
parseRule = do
    a <- pDecimal
    P.char '|'
    b <- pDecimal
    pure (a, b)

parseInput :: CharParser ([(Int, Int)], [[Int]])
parseInput = do
    rules <- sepEndByLines parseRule
    _ <- P.newline
    pages <- sepByLines $ pDecimal `sepBy'` ","
    pure (rules, pages)

parseInput' :: CharParser ([V2 Int], [[Int]])
parseInput' = do
    rules <- sepEndByLines $ V2 pDecimal pDecimal `sequenceSepBy` "|"
    _ <- P.newline
    pages <- sepByLines $ pDecimal `sepBy'` ","
    pure (rules, pages)

sortFirst :: ([(Int, Int)], [[Int]]) -> [([Int], [Int])]
sortFirst (rules, pages) = [(sortByRules rules page, page) | page <- pages]

sortFirst' :: ([V2 Int], [[Int]]) -> [([Int], [Int])]
sortFirst' (rules, pages) = [(sortByRules' rules page, page) | page <- pages]

sortByRules :: [(Int, Int)] -> [Int] -> [Int]
sortByRules rules = \xs ->
    G.topsort . G.nfilter (`S.member` S.fromList xs) $ rulesGraph
  where
    rulesGraph :: G.Gr () ()
    rulesGraph =
        G.mkUGraph (nubOrd $ foldMap toList rules) rules

sortByRules' :: [V2 Int] -> [Int] -> [Int]
sortByRules' rules = \xs ->
    G.topsort . G.nfilter (`S.member` S.fromList xs) $ rulesGraph
  where
    rulesGraph :: G.Gr () ()
    rulesGraph =
        G.mkUGraph (nubOrd $ foldMap toList rules) [(x, y) | V2 x y <- rules]

partI :: IO ()
partI = do
    test <- readFile "data/2024/test-day5.txt"
    P.parseTest (sortFirst' <$> parseInput') test
    P.parseTest (sortFirst <$> parseInput) test
