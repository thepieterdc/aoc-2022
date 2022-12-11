module Day11.Common (parse, run, test) where

import           Data.List    (sortOn)
import           Data.Maybe   (fromMaybe)
import           Data.Ord     (Down (Down))
import           Prelude      hiding (until)
import           Utils.Lists  (mapIdx)
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

processed :: Monkey -> Int
processed (Monkey _ _ _ _ _ p) = p

target :: Monkey -> Int -> Int
target (Monkey _ _ t ifTrue ifFalse _) i = if i `mod` t == 0 then ifTrue else ifFalse

test :: Monkey -> Int
test (Monkey _ _ t _ _ _) = t

transfer :: [Monkey] -> Int -> Int -> [Monkey]
transfer [] _ _ = []
transfer (m@(Monkey is o t tt tf p):ms) dest item = if dest == 0
    then Monkey (is ++ [item]) o t tt tf p : ms
    else m : transfer ms (dest - 1) item

run :: (Int -> Int) -> Int -> [Monkey] -> Int
run afterProcess runs = product . take 2 . sortOn Down . map processed . (!! runs) . iterate (run' afterProcess 0)

run' :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
run' afterProcess idx monkeys = if idx >= length monkeys then monkeys else
    let monkey@(Monkey items op t tt tf p) = monkeys !! idx
        newMonkeys = foldl (uncurry . transfer) monkeys (map ((\i -> (target monkey i, i)) . afterProcess . op) items)
    in run' afterProcess (idx + 1) (mapIdx idx (const (Monkey [] op t tt tf (p + length items))) newMonkeys)
