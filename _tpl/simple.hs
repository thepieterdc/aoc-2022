module Main where

import           System.Environment (getArgs)

import           Utils.Filtering    (isNumber)
import           Utils.Parsing      (parseInt)

findMax :: Int -> Int -> [String] -> Int
findMax m c [] = max m c
findMax m c (x:xs) =   if isNumber x
    then findMax m (c + parseInt x) xs
    else findMax (max m c) 0 xs

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ lines contents
