module Main where

import           System.Environment (getArgs)

score :: String -> Int
score (op:_:m:_) = case (op,m) of
    ('A', 'X') -> 3 + 0 -- Lose: Rock vs Scissors
    ('A', 'Y') -> 1 + 3 -- Draw: Rock vs Rock
    ('A', 'Z') -> 2 + 6 -- Win: Rock vs Paper
    ('B', 'X') -> 1 + 0 -- Lose: Paper vs Rock
    ('B', 'Y') -> 2 + 3 -- Draw: Paper vs Paper
    ('B', 'Z') -> 3 + 6 -- Win: Paper vs Scissors
    ('C', 'X') -> 2 + 0 -- Lose: Scissors vs Paper
    ('C', 'Y') -> 3 + 3 -- Draw: Scissors vs Scissors
    ('C', 'Z') -> 1 + 6 -- Win: Scissors vs Rock
    _          -> 0
score _ = 0

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ sum (map score (lines contents))
