module Day1.Common (run) where

import           Utils.Filtering (countWhere)
import           Utils.Parser    (Parser, digits, doParse, integer, optional,
                                  some, token)

parse :: Parser [Int]
parse = do
    groups <- some parseGroup
    last <- parseLastGroup
    return $ groups ++ [last]

parseGroup :: Parser Int
parseGroup = do
    n <- some parseNumber
    _ <- token '\n'
    return $ sum n

parseLastGroup :: Parser Int
parseLastGroup = do
    n <- some parseNumber
    _ <- optional "\n"
    return $ sum n

parseNumber :: Parser Int
parseNumber = do
    n <- integer
    _ <- token '\n'
    return n

run :: String -> [Int]
run = doParse parse
