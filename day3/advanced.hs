module Main where

import           System.Environment (getArgs)

import           Data.List          (elemIndex, intersect)
import           Data.Maybe         (fromJust)

findBadges :: [String] -> [Char]
findBadges [] = []
findBadges lines = findBadge search:findBadges rest
    where (search,rest) = splitAt 3 lines

findBadge :: [String] -> Char
findBadge [a,b,c] = head (a `intersect` b `intersect` c)

prioritise :: Char -> Int
prioritise c = fromJust (elemIndex c (['a'..'z'] ++ ['A'..'Z'])) + 1

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ sum $ map prioritise (findBadges (lines contents))
