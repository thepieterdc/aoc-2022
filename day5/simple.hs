module Main where

import           Data.List          (transpose)
import           System.Environment (getArgs)
import           Utils.Parser       (Parser, char, digits, doParse, many,
                                     optional, some, string, token, void, (<|>))
import           Utils.Parsing      (parseInt)

--
-- Types.
--

-- Move Amount From To
data Move = Move Int Int Int deriving (Eq, Show)

type Row = [Maybe Char]

--
-- Parsing.
--

parse :: Parser ([Row], [Move])
parse = do
    rows <- some parseRow
    _ <- parseNames
    _ <- token '\n'
    moves <- some parseMove
    return (transpose rows, moves)

parseRow :: Parser Row
parseRow = do {cells <- some parseCell; _ <- token '\n'; return cells}

parseCell :: Parser (Maybe Char)
parseCell = do {_ <- optional " "; parseFilledCell <|> parseEmptyCell}

parseEmptyCell :: Parser (Maybe Char)
parseEmptyCell = do {_ <- string "   "; return Nothing}

parseFilledCell :: Parser (Maybe Char)
parseFilledCell = do {_ <- token '['; c <- char; _ <- token ']'; return (Just c)}

parseNames :: Parser ()
parseNames = do {_ <- token ' '; _ <- some (digits <|> string "   "); _ <- string " \n"; return ()}

parseMove :: Parser Move
parseMove = do
    _ <- string "move "
    amt <- digits
    _ <- string " from "
    start <- digits
    _ <- string " to "
    dest <- digits
    _ <- token '\n'
    -- Convert start and dest to use zero-based indexing.
    return $ Move (parseInt amt) (parseInt start - 1) (parseInt dest - 1)

--
-- Utils
--

applyMove :: Move -> [Row] -> [Row]
applyMove (Move amt start dest) rows = inserted where
    (extracted, remaining) = extract amt start rows
    inserted = insert extracted dest remaining

extract :: Int -> Int -> [Row] -> ([Maybe Char], [Row])
extract amount pos rows = (extracted, remaining) where
    extracted = take amount (rows !! pos)
    remaining = take pos rows ++ drop amount (rows !! pos) : drop (pos + 1) rows

insert :: [Maybe Char] -> Int -> [Row] -> [Row]
insert part pos rows = take pos rows ++ inserted : drop (pos + 1) rows where
    -- Add reverse to support moving a whole column at once for performance.
    inserted = reverse part ++ filter (/= Nothing) (rows !! pos)

solve :: [Row] -> [Move] -> [Row]
solve rows = foldl (flip applyMove) (map (dropWhile (== Nothing)) rows)

translate :: Maybe Char -> String
translate (Just c) = c : ""
translate Nothing  = ""

--
-- The rows are transposed to better facilitate operations.
--
main :: IO ()
main = do
    file:_ <- getArgs
    contents <- readFile file
    print $ concatMap (translate. head) $ uncurry solve (doParse parse contents)
