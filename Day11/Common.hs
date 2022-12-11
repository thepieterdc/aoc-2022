module Day11.Common (parse, Monkey, clearItems, incProcessed, items, process, processed, target, transfer) where

import           Data.Maybe   (fromMaybe)
import           Prelude      hiding (until)
import           Utils.Parser (Parser, char, doParse, integer, many, optional,
                               some, string, token, until, void, (<|>))

--
-- Types
--

-- (StartingItems, Operation, Test, IfTrue, IfFalse, Processed)
data Monkey = Monkey [Int] Operation Int Int Int Int deriving (Show)

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
    return $ Monkey si op t tt tf 0

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

--
-- Utils
--

addItem :: Monkey -> Int -> Monkey
addItem (Monkey is o t tt tf p) i = Monkey (is ++ [i]) o t tt tf p

clearItems :: Monkey -> Monkey
clearItems (Monkey is o t tt tf p) = Monkey [] o t tt tf p

incProcessed :: Int -> Monkey -> Monkey
incProcessed amt (Monkey is o t tt tf p) = Monkey is o t tt tf (p + amt)

items :: Monkey -> [Int]
items (Monkey a _ _ _ _ _) = a

process :: Monkey -> (Int -> Int)
process (Monkey _ op _ _ _ _) = op

processed :: Monkey -> Int
processed (Monkey _ _ _ _ _ p) = p

target :: Monkey -> Int -> Int
target (Monkey _ _ test ifTrue ifFalse _) i = if i `mod` test == 0 then ifTrue else ifFalse

transfer :: [Monkey] -> Int -> Int -> [Monkey]
transfer [] _ _ = []
transfer (m:ms) dest item = if dest == 0
    then addItem m item : ms
    else m : transfer ms (dest - 1) item
