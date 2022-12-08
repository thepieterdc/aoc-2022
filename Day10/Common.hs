module Day10.Common (Operation(Add, Noop), run) where

import           Utils.Parser (Parser, char, doParse, integer, some, string,
                               token, (<|>))

data Operation = Add Int | Noop deriving Show

eval :: Int -> [Operation] -> [Int]
eval val []            = []
eval val ((Add i):ops) = val : val : eval (val + i) ops
eval val (Noop:ops)    = val : eval val ops

parse :: Parser [Operation]
parse = some (parseNoop <|> parseAdd) where
    parseNoop = do {string "noop\n"; return Noop}
    parseAdd = do {string "addx "; n <- integer; token '\n'; return $ Add n}

run :: String -> [Int]
run = eval 1 . doParse parse
