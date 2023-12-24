{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day7 where

import Control.Applicative (many, (<|>))
import Control.Lens.Each (each)
import Control.Lens.Fold (toListOf)
import Data.Attoparsec.Text
import Data.List (sort, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO

data Card = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
    deriving stock (Eq, Ord, Enum, Bounded)

instance Show Card where
    show A = "A"
    show K = "K"
    show Q = "Q"
    show J = "J"
    show T = "T"
    show Nine = "9"
    show Eight = "8"
    show Seven = "7"
    show Six = "6"
    show Five = "5"
    show Four = "4"
    show Three = "3"
    show Two = "2"
    show One = "1"

data Type = HighCard | OnePair | TwoPair | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind
    deriving stock (Show, Eq, Ord, Enum, Bounded)

newtype Hand = Hand {getCards :: (Card, Card, Card, Card, Card)}
newtype Hand' = Hand' {getCards' :: (Card, Card, Card, Card, Card)}

data Row = Row Hand Int deriving (Eq, Show)
data Row' = Row' Hand' Int deriving (Eq, Show)

rowToRow' :: Row -> Row'
rowToRow' (Row h b) = Row' (handToHand' h) b

type Input = [Row]

handToHand' :: Hand -> Hand'
handToHand' = Hand' . getCards

hand'ToHand :: Hand' -> Hand
hand'ToHand = Hand . getCards'

compareHand :: Hand -> Hand -> Ordering
compareHand a b = if typeA == typeB then compare (handToList a) (handToList b) else typeA `compare` typeB
  where
    typeA = getType a
    typeB = getType b

compareHand' :: Hand' -> Hand' -> Ordering
l `compareHand'` r = if tL == tR then compareCard (handToList $ hand'ToHand l) (handToList $ hand'ToHand r) else tL `compare` tR
  where
    tL = getTypeWithJoker l
    tR = getTypeWithJoker r

    compareCard :: [Card] -> [Card] -> Ordering
    compareCard [] [] = EQ
    compareCard (J : ls) (J : rs) = compareCard ls rs
    compareCard (J : _) _ = LT
    compareCard (a : ls) (b : rs) = if a == b then compareCard ls rs else compare a b
    compareCard _ _ = error "cannot compare"

instance Show Hand where
    show = concatMap show . handToList

instance Eq Hand where
    a == b = a `compareHand` b == EQ

instance Ord Hand where
    a `compare` b = a `compareHand` b

instance Ord Row where
    (Row a _) `compare` (Row b _) = a `compare` b

instance Show Hand' where
    show = show . hand'ToHand

instance Eq Hand' where
    a == b = a `compareHand'` b == EQ

instance Ord Hand' where
    a `compare` b = a `compareHand'` b

instance Ord Row' where
    (Row' a _) `compare` (Row' b _) = a `compare` b

handToList :: Hand -> [Card]
handToList = toListOf each . getCards

cardParser :: Parser Card
cardParser =
    (char 'A' >> return A)
        <|> (char 'K' >> return K)
        <|> (char 'Q' >> return Q)
        <|> (char 'J' >> return J)
        <|> (char 'T' >> return T)
        <|> (char '9' >> return Nine)
        <|> (char '8' >> return Eight)
        <|> (char '7' >> return Seven)
        <|> (char '6' >> return Six)
        <|> (char '5' >> return Five)
        <|> (char '4' >> return Four)
        <|> (char '3' >> return Three)
        <|> (char '2' >> return Two)
        <|> (char '1' >> return One)

handParser :: Parser Hand
handParser = do
    a <- cardParser
    b <- cardParser
    c <- cardParser
    d <- cardParser
    e <- cardParser
    return $ Hand (a, b, c, d, e)

rowParser :: Parser Row
rowParser = do
    h <- handParser
    _ <- space
    i <- decimal
    return $ Row h i

inputParser :: Parser Input
inputParser = many $ rowParser <* endOfLine

getType :: Hand -> Type
getType hand = case sortBy (flip compare) . M.elems . foldCard $ hand of
    5 : _ -> FiveOfKind
    4 : _ -> FourOfKind
    3 : 2 : _ -> FullHouse
    3 : _ -> ThreeOfKind
    2 : 2 : _ -> TwoPair
    2 : _ -> OnePair
    _ -> HighCard
  where
    foldCard :: Hand -> Map Card Int
    foldCard = foldl (\m c -> M.insertWith (+) c 1 m) M.empty . handToList

getTypeWithJoker :: Hand' -> Type
getTypeWithJoker = beforeType . addJoker . foldCard
  where
    addJoker :: (Map Card Int, Int) -> [Int]
    addJoker (m', c) = let a = sortBy (flip compare) (M.elems m') in if null a then [c] else (head a + c) : tail a
    foldCard :: Hand' -> (Map Card Int, Int)
    foldCard = foldl (\(m, accum) c -> if c == J then (m, accum + 1) else (M.insertWith (+) c 1 m, accum)) (M.empty, 0) . handToList . hand'ToHand
    beforeType :: [Int] -> Type
    beforeType v = case v of
        5 : _ -> FiveOfKind
        4 : _ -> FourOfKind
        3 : 2 : _ -> FullHouse
        3 : _ -> ThreeOfKind
        2 : 2 : _ -> TwoPair
        2 : _ -> OnePair
        _ -> HighCard

partI :: IO ()
partI = do
    rawInput <- TIO.readFile "data/2023/day7-test.txt"
    let e = (parseOnly inputParser rawInput)
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print (foldl (\accum (Row _ bid, rank) -> accum + bid * rank) 0 $ zip (sort input) [1 ..])

partII :: IO ()
partII = do
    rawInput <- TIO.readFile "data/2023/day7-test.txt"
    let e = (parseOnly inputParser rawInput)
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print (foldl (\accum (Row' _ bid, rank) -> accum + bid * rank) 0 $ zip (sort $ map rowToRow' input) [1 ..])
