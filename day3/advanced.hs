module Main where

import           System.Environment (getArgs)

import           Data.List          (elemIndex, intersect)
import           Data.Maybe         (fromJust)

findBadges :: [String] -> [Char]
findBadges [] = []
findBadges lines = head (foldl1 intersect search):findBadges rest
    where (search,rest) = splitAt 3 lines

prioritise :: Char -> Int
prioritise c = fromJust (elemIndex c (['a'..'z'] ++ ['A'..'Z'])) + 1

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ sum $ map prioritise (findBadges (lines contents))
