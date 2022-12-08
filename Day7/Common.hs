module Day7.Common (Tree(Dir, File), run, size) where

import           Data.Maybe   (catMaybes, fromJust, listToMaybe)
import           Prelude      hiding (until)
import           Utils.Lists  (nothings)
import           Utils.Parser (Parser, doParse, end, guard, integer, many, some,
                               string, token, until, void, (<|>))

--
-- Types.
--

data Tree = Dir String [Tree] | File String Int deriving (Show)

--
-- Parsing.
--

parse :: Parser Tree
parse = parseDirectory

parseDirectory :: Parser Tree
parseDirectory = do
    string "$ cd "
    name <- until '\n'
    string "$ ls\n"
    contents <- many parseDirectoryContents
    subDirectories <- many parseDirectory
    guard (nothings contents == length subDirectories)
    void (string "$ cd ..\n") <|> end
    return $ Dir name (catMaybes contents ++ subDirectories)

parseDirectoryContents :: Parser (Maybe Tree)
parseDirectoryContents = parseFile <|> parseDirectoryName where
    parseDirectoryName = string "dir" >> until '\n' >> return Nothing
    parseFile = do {size <- integer; token ' '; n <- until '\n'; return $ Just (File n size)}

--findDirectorySizes :: Tree -> [Int]
--findDirectorySizes d@(Dir _ fs) = size d : concatMap findDirectorySizes fs
-- we only care about directories.
--findDirectorySizes _            = [0]

run :: String -> Tree
run = doParse parse

size :: Tree -> Int
size (Dir _ contents) = (sum . map size) contents
size (File _ s)       = s
