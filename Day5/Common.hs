module Day5.Common (run) where

import           Data.List     (transpose)
import           Data.Maybe    (mapMaybe)
import           Utils.Lists   (mapIdx)
import           Utils.Parser  (Parser, char, digits, doParse, many, optional,
                                some, string, token, void, (<|>))
import           Utils.Parsing (parseInt)

--
-- Types.
--

-- Function that modifies the inserted row.
type InsertHook = [Maybe Char] -> [Maybe Char]

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

applyMove :: InsertHook -> Move -> [Row] -> [Row]
applyMove afterInsert (Move amt start dest) rows = inserted where
    (extracted, remaining) = extract amt start rows
    inserted = insert afterInsert extracted dest remaining

extract :: Int -> Int -> [Row] -> ([Maybe Char], [Row])
extract amount pos rows = (extracted, remaining) where
    (extracted, remainingRow) = splitAt amount (rows !! pos)
    remaining = mapIdx pos (const remainingRow) rows

insert :: InsertHook -> [Maybe Char] -> Int -> [Row] -> [Row]
insert afterInsert part pos rows = mapIdx pos (const inserted) rows where
    inserted = afterInsert part ++ filter (/= Nothing) (rows !! pos)

solve :: InsertHook -> [Row] -> [Move] -> [Row]
solve afterInsert rows = foldl (flip (applyMove afterInsert)) (map (dropWhile (== Nothing)) rows)

--
-- The rows are transposed to better facilitate operations.
--
run :: InsertHook -> String -> String
run afterInsert contents = mapMaybe head $ uncurry (solve afterInsert) (doParse parse contents)
