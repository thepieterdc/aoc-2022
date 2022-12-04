module Main where

import           System.Environment (getArgs)
import           Utils.Interval     (contains, overlaps)
import           Utils.Parser       (Parser, digits, doParse, token)
import           Utils.Parsing      (parseInt)

_contains :: ((Int, Int), (Int, Int)) -> Bool
_contains (fst, snd) = contains fst snd || contains snd fst

parse :: Parser ((Int, Int), (Int, Int))
parse = do
    l <- pair
    _ <- token ','
    r <- pair
    return (l, r)

pair :: Parser (Int, Int)
pair = do
    start <- digits
    _ <- token '-'
    end <- digits
    return (parseInt start, parseInt end)

main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ length $ filter _contains $ map (doParse parse) (lines contents)
