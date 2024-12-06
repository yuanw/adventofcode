{-# LANGUAGE OverloadedStrings #-}

module Y2024.Day3 where

import Text.Megaparsec qualified as P

mulParser :: Parser (Int, Int)
mulParser = do
    _ <- string "mul("
    a <- decimal
    _ <- char ','
    b <- decimal
    _ <- string ")"
    return (a, b)

-- P.try . P.skipManyTill P.anySingle . P.try
-- mulsParser :: Parser [(Int, Int)]
-- mulsParser = try .
