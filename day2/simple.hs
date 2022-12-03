module Main where

import           System.Environment (getArgs)

score :: String -> Int
score (op:_:m:_) = case (op,m) of
    ('A', 'X') -> 1 + 3 -- Rock vs Rock
    ('A', 'Y') -> 2 + 6 -- Rock vs Paper
    ('A', 'Z') -> 3 + 0 -- Rock vs Scissors
    ('B', 'X') -> 1 + 0 -- Paper vs Rock
    ('B', 'Y') -> 2 + 3 -- Paper vs Paper
    ('B', 'Z') -> 3 + 6 -- Paper vs Scissors
    ('C', 'X') -> 1 + 6 -- Scissors vs Rock
    ('C', 'Y') -> 2 + 0 -- Scissors vs Paper
    ('C', 'Z') -> 3 + 3 -- Scissors vs Scissors
    _          -> 0
score _ = 0

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ sum (map score (lines contents))
