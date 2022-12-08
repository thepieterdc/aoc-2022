module Day8.Advanced where

import           Data.List   (groupBy, sort, transpose)
import           Day8.Common (Cell (Cell), column, height, parseGrid, row)
import           Utils.IO    (loadInput)

getScore :: [(Cell, Int)] -> Cell -> [(Cell, Int)]
getScore [] c = [(c, 0)]
getScore cs c = (c, getScore' cs) : cs where
    getScore' [] = 0
    getScore' (c':cs') = if height (fst c') < height c
        then 1 + getScore' cs'
        else 1

maxScore :: [(Cell, Int)] -> Int
maxScore cs = maximum $ map groupScore (groupBy cmp cs) where
    cmp x y = (column (fst x) == column (fst y)) && (row (fst x) == row (fst y))
    groupScore group = product $ map snd group

-- Filter out 1 since these don't have an impact on multiplication.
score :: [[Cell]] -> [(Cell, Int)]
score cells = filter (\s -> snd s /= 1) $ concat (scoreEW ++ scoreNS ++ scoreSN ++ scoreWE) where
    scoreEW = map (foldl getScore []) cells
    scoreNS = map (foldl getScore []) (transpose cells)
    scoreSN = map (foldl getScore [] . reverse) (transpose cells)
    scoreWE = map (foldl getScore [] . reverse) cells

main :: IO ()
main = loadInput >>= print . maxScore . sort . score . parseGrid . lines
