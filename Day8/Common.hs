module Day8.Common (Cell(Cell), column, height, parseGrid, row) where

-- (x, y, height)
data Cell = Cell Int Int Int deriving (Eq, Ord, Show)

column :: Cell -> Int
column (Cell c _ h) = c

height :: Cell -> Int
height (Cell _ _ h) = h

row :: Cell -> Int
row (Cell _ r _) = r

parseGrid :: [String] -> [[Cell]]
parseGrid = parseGrid' 0 where
        parseGrid' _ [] = []
        parseGrid' i (r:rs) = [Cell c i (read (h : "") :: Int) | (c, h) <- zip [0..(length r)] r] : parseGrid' (i + 1) rs
