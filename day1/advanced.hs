module Main where

import           System.Environment (getArgs)

import           Data.List          (reverse, sortOn)
import           Data.Ord           (Down (Down))
import           Utils.Filtering    (isNumber)
import           Utils.Parsing      (parseInt)


calculateSums :: [String] -> Int -> [Int]
calculateSums [] result = [result]
calculateSums (x:xs) c = if isNumber x
    then calculateSums xs (parseInt x + c)
    else c : calculateSums xs 0

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ sum $ take 3 $ sortOn Down $ calculateSums (lines contents) 0
