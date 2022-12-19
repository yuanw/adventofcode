{-# LANGUAGE TemplateHaskell #-}

module Y2022.Day11 where
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Control.Applicative (many, (<|>))
import Control.Lens hiding (index, op)
import Control.Lens.TH
import Data.Attoparsec.Text (
    Parser,
    char,
    decimal,
    digit,
    double,
    endOfLine,
    many1,
    parseOnly,
    sepBy,
    signed,
    string,
 )

import Data.Either (rights)
import Data.Text qualified as T
type Idx = Int
type Val = Int
data Monkey = Monkey
    { _index :: Int
    , _items :: [Val]
    , _op :: Val -> Val
    , _divisible :: Val
    , _trueThrow :: Idx
    , _falseThrow :: Idx
    , _count :: Int
    }
$(makeLenses ''Monkey)
instance Show Monkey where
    show m = "Monkey " ++ show (_index m) ++ " inspected items " ++ show (_count m) ++ " times. :" ++ show (_items m)
eval :: Int -> [Monkey] -> [Monkey]
eval i monkeys =
    let monkey = monkeys !! i
     in if null (view items monkey)
            then monkeys
            else
                let item = head (view items monkey)
                    newVal = ((view op monkey) item) `div` 3
                    m' = throwItem newVal (monkeys !! (if newVal `mod` (view divisible monkey) == 0 then view trueThrow monkey else view falseThrow monkey))
                 in eval i (monkeys & element (_index m') .~ m' & element (view index monkey) .~ ((over count (+ 1)) . (over items tail) $ monkey))
replicateList :: Int -> [a] -> [a]
replicateList 0 _ = []
replicateList n list = list ++ replicateList (n - 1) list

monkeyParser :: Parser Monkey
monkeyParser = do
    ind <- string "Monkey " >> decimal <* string ":" <* endOfLine
    start <- string "  Starting items: " >> (decimal `sepBy` string ", ") <* endOfLine
    -- op' <- string "  Operation: new = old " >> (string "* " >> return (*)) <|> (string "+ " >> return (+))
    -- opVal <- decimal <* endOfLine
    -- d <- string "  Test: divisible by " >> decimal <* endOfLine
    -- tt <- string "    If true: throw to monkey " >> decimal <* endOfLine
    -- ft <- string "    If false: throw to monkey " >> decimal <* endOfLine
    return (Monkey ind start (* 19) 1 1 1 0)

throwItem :: Val -> Monkey -> Monkey
throwItem v = over items (++ [v])

testMonkeys :: [Monkey]
testMonkeys =
    [ Monkey 0 [79, 98] (* 19) 23 2 3 0
    , Monkey 1 [54, 65, 75, 74] (+ 6) 19 2 0 0
    , Monkey 2 [79, 60, 97] (\x -> x * x) 13 1 3 0
    , Monkey 3 [74] (+ 3) 17 0 1 0
    ]

test :: IO ()
test = do
    input <-  splitOn "\n\n" <$>  readFile "data/2022/day11-test.txt"
    mapM_ print input
    let monkeys =  map  (  parseOnly monkeyParser  . T.pack  )  $ input
    print monkeys
    -- mapM_ print $ foldl (flip eval) testMonkeys (replicateList 20 [0, 1, 2, 3])
