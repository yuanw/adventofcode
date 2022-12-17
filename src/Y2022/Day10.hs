
module Y2022.Day10 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (
    Parser,
    char,
    digit,
    endOfLine,
    letter,
    many1,
    parseOnly,
    space,
    string,
 )

import Data.Either (fromRight)
import Data.Text qualified as T
data Instruction = Noop | AddX Int deriving stock Show

instructionParser :: Parser Instruction
instructionParser = (string "noop" >> return Noop) <|> (string "addx " >> AddX . read <$> many1 digit ) <|> (string "addx -" >> AddX .  ( 0 - ) . read <$> many1 digit  )

parseInstructions :: Parser [Instruction]
parseInstructions = many ( instructionParser <* endOfLine)


nthCycle :: Int -> [Instruction] -> Int
nthCycle n instructions = let r = if n >= length results then results else take n results in n * (last r)
  where results =  eva instructions



eva :: [Instruction] -> [Int]
eva = tail . foldl (\ results instruction -> let result = last results in case instruction of
                                        Noop -> results ++ [result]
                                        AddX n -> results ++ [result, result + n] )
      [1]

test :: IO ()
test = do
    input <- readFile "data/2022/day10-test.txt"
    let instructions = fromRight [] $ parseOnly parseInstructions $ T.pack input
        -- r = nthCycle 5 [Noop, AddX 3, AddX (-5)]
    print instructions
    print $ nthCycle 20 instructions
    print $ nthCycle 60 instructions
    print $ nthCycle 100 instructions
    print $ nthCycle 140 instructions
    print $ nthCycle 180 instructions
    print $ nthCycle 220 instructions
    print . length $ eva instructions
        -- r = moveRope ((0,1): (tail empty'))
    -- print movements
