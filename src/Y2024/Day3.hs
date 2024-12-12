{-# LANGUAGE OverloadedStrings #-}

module Y2024.Day3 where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC

type StringParser = P.Parsec Void String

mulParser :: StringParser (Int, Int)
mulParser = do
    _ <- PC.string "mul("
    a <- P.decimal
    _ <- PC.char ','
    b <- P.decimal
    _ <- PC.string ")"
    return (a, b)

-- P.try . P.skipManyTill P.anySingle . P.try
-- mulsParser :: Parser [(Int, Int)]
-- mulsParser = try .
