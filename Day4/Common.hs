module Day4.Common (run) where

import           Utils.Filtering (countWhere)
import           Utils.Parser    (Parser, digits, doParse, integer, optional,
                                  some, token)

parse :: Parser [((Int, Int), (Int, Int))]
parse = some parseLine

parseLine :: Parser ((Int, Int), (Int, Int))
parseLine = do
    l <- pair
    _ <- token ','
    r <- pair
    _ <- optional "\n"
    return (l, r)

pair :: Parser (Int, Int)
pair = do
    start <- integer
    _ <- token '-'
    end <- integer
    return (start, end)

run :: (((Int, Int), (Int, Int)) -> Bool) -> String -> Int
run cmp input = countWhere cmp $ doParse parse input
