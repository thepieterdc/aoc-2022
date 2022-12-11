module Day11.Common (parse) where

import           Data.Maybe   (fromMaybe)
import           Prelude      hiding (until)
import           Utils.Parser (Parser, char, doParse, integer, many, optional,
                               some, string, token, until, void, (<|>))

--
-- Types
--

-- (StartingItems, Operation, Test, IfTrue, IfFalse)
type Monkey = ([Int], Operation, Int, Int, Int)

type Operation = (Int -> Int)

instance Show Operation where
    show :: Operation -> String
    show _ = "_operation_"

--
-- Parsing
--

parse :: String -> [Monkey]
parse = doParse (some parseMonkey)

parseMonkey :: Parser Monkey
parseMonkey = do
    string "M"
    until '\n'
    si <- parseStartingItems
    op <- parseOperation
    t <- parseTest
    tt <- parseTestBranch
    tf <- parseTestBranch
    optional "\n"
    return (si, op, t, tt, tf)

parseOperation :: Parser (Int -> Int)
parseOperation = do {string "  O"; until '='; fn <- parse'; token '\n'; return fn} where
    opnd = (Just <$> integer) <|> (string "old" >> return Nothing)
    optr = (token '*' >> return (*)) <|> (token '+' >> return (+))
    parse' = do {token ' '; l <- opnd; token ' '; op <- optr; token ' '; r <- opnd; return (\a -> op (fromMaybe a l) (fromMaybe a r))}

parseStartingItems :: Parser [Int]
parseStartingItems = do {string "  S"; until ':'; many number} where
    number = do {token ' '; n <- integer; token ',' <|> token '\n'; return n}

parseTest :: Parser Int
parseTest = do {until 'y'; token ' '; n <- integer; token '\n'; return n}

parseTestBranch :: Parser Int
parseTestBranch = do {until ':'; string " throw to monkey "; n <- integer; token '\n'; return n}
