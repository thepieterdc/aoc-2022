module Main where

import           System.Environment (getArgs)

import           Data.List          (elemIndex, intersect)
import           Data.Maybe         (fromJust)
import           Utils.Filtering    (isNumber)
import           Utils.Parsing      (parseInt)

compartments :: String -> (String, String)
compartments line = splitAt half line
    where half = length line `div` 2

findDuplicate :: (String, String) -> Char
findDuplicate (left, right) = head (left `intersect` right)

prioritise :: Char -> Int
prioritise c = fromJust (elemIndex c (['a'..'z'] ++ ['A'..'Z'])) + 1

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ sum $ map (prioritise . findDuplicate . compartments) (lines contents)
