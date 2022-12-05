module Main where

import           System.Environment (getArgs)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ lines contents
