{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2022.Day7 where
import Data.List (break)
import Data.Attoparsec.Text hiding (take)
import Data.Text qualified as T
import Control.Applicative (many, (<|>))
import Data.Either (fromRight)
data FileSystem = Directory {   _dName :: String, _content :: [FileSystem] } | File { _fName :: String , _size :: Int } deriving stock Show

getSize :: FileSystem -> Int
getSize (File _ s ) = s
getSize (Directory _ []  ) = 0
getSize (Directory n (x:xs)  ) = getSize x + getSize (Directory n xs  )
type Name = String
type Size = Int
data Output = CD String | LS | Dir String | FL Int String deriving stock Show

data FSCrumb = FSCrumb { _parent :: Name, _before :: [FileSystem] , _after :: [FileSystem]} deriving stock Show
type Zipper = (FileSystem, [FSCrumb])

parserFile :: Parser Output
parserFile = do
  size <- many1 digit
  _ <- space
  name <- many1 (letter <|> char '.' <|> digit)
  return $ FL (read size)  name

outputParser :: Parser Output
-- outputParser = (string "$ ls" >> return LS) <|> (string "$ cd " >>  CD <$> manyTill anyChar (string "\n") )  <|> (string "dir " >>  Dir <$> manyTill anyChar (string "\n") ) <|> parserFile
outputParser = (string "$ ls" >> return LS) <|> (string "$ cd " >>  CD  <$> many (letter <|> char '/' <|> char '.' ))  <|> (string "dir " >>  Dir  <$> many (letter <|> digit)) <|> parserFile

outputsParser :: Parser [Output]
outputsParser = many (outputParser <* endOfLine   )

empty :: Zipper
empty = (Directory "/" [], [FSCrumb "/" [] []])

test :: IO ()
test = do

    -- input <- readFile "data/2022/day7-test.txt"
    -- let outputs = fromRight [] $ parseOnly outputsParser $ T.pack input
    --     tree =  foldl buildTree empty (tail outputs)
    -- print tree
    let tree = foldl buildTree (Directory "a" [], [FSCrumb "/" [] []]) [ FL 14 "b.txt", FL 85 "c.dat", Dir "d" , CD "a", LS ] --, Dir "e"]
    print tree
    -- print $ parseOnly outputParser "$ cd /"
    -- print $ parseOnly outputParser "$ ls"
    -- print $ parseOnly outputParser "14779 cmss"
    -- print $ parseOnly outputParser "dir ctctt"
    -- print $ parseOnly outputParser "101350 gpbswq.njr"
    -- print $ parseOnly outputParser "270744 mglrchsr"

buildTree :: Zipper ->  Output -> Zipper
buildTree z LS = z
buildTree (item , FSCrumb name ls rs : bs) (Dir folderName) = (Directory folderName [], FSCrumb name (ls ++ [item]) rs  :bs)
buildTree (item, FSCrumb name ls rs : bs) (FL size fileName) = (File fileName size, FSCrumb name (ls ++ [item]) rs : bs)
buildTree z (CD "..") = goUp z
buildTree z (CD name) = goTo name z
buildTree _ _ = error ""

goTo :: String -> Zipper -> Zipper
goTo name (item, FSCrumb folderName ls rs : bs) = let (ls', rs') = break (nameIs name) (ls ++ (item : rs)) in (head rs', FSCrumb name ls' (tail rs') : FSCrumb folderName ls rs : bs)

goUp :: Zipper -> Zipper
goUp (item, FSCrumb name ls rs : bs) = (Directory name (ls ++ item : rs), bs)

nameIs :: String -> FileSystem -> Bool
nameIs name (Directory folderName _) = name == folderName
nameIs name (File fileName _ ) = name == fileName

topMost :: Zipper -> FileSystem
topMost (item, [ FSCrumb name ls rs  ]) = Directory name (ls ++ item : rs)
topMost z = topMost ( goUp z)
