{-# LANGUAGE OverloadedStrings #-}

module Y2024.Day3 where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Text.Megaparsec.Char.Lexer qualified as PL

type CharParser = P.Parsec Void String

parseMaybeLenient :: P.Parsec Void s a -> s -> Maybe a
parseMaybeLenient p = eitherToMaybe . P.parse p "parseMaybeLenient"

pDropUntil :: (P.Stream s, Ord e) => P.Parsec e s end -> P.Parsec e s end
pDropUntil = P.try . P.skipManyTill P.anySingle . P.try

-- | Alias for 'parseMaybeLenient'
parseMaybe' :: P.Parsec Void s a -> s -> Maybe a
parseMaybe' = parseMaybeLenient

parseMul :: CharParser Int
parseMul = product <$> P.between "mul(" ")" (replicate 2 PL.decimal `sequenceSepBy` ",")
