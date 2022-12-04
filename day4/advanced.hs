module Main where

import           System.Environment (getArgs)
import           Utils.Parser       (Parser, digits, doParse, many, token)
import           Utils.Parsing      (parseInt)

contains :: ((Int, Int), (Int, Int)) -> Bool
contains ((a, b), (c, d)) = (a >= c && a <= d) || (b >= c && b <= d) || (c >= a && c <= b) || (d <= a && d >= b)

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
    print $ length $ filter contains $ map (doParse parse) (lines contents)
