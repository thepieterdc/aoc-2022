module Day8.Simple where

import           Data.List   (transpose)
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Day8.Common (Cell, height, parseGrid)
import           Utils.IO    (loadInput)

-- Go in all 4 directions, skip the edges
visible :: [[Cell]] -> Set Cell
visible cells = Set.unions [visibleNS cells, visibleEW cells, visibleSN cells, visibleWE cells]

visibleEW :: [[Cell]] -> Set Cell
visibleEW cells = Set.fromList $ concatMap (foldl isVisible [] . reverse) cells

visibleNS :: [[Cell]] -> Set Cell
visibleNS cells = Set.fromList $ concatMap (foldl isVisible [] . reverse) (transpose cells)

visibleSN :: [[Cell]] -> Set Cell
visibleSN cells = Set.fromList $ concatMap (foldl isVisible []) (transpose cells)

visibleWE :: [[Cell]] -> Set Cell
visibleWE cells = Set.fromList $ concatMap (foldl isVisible []) cells

isVisible :: [Cell] -> Cell -> [Cell]
isVisible [] c      = [c]
isVisible (c':cs) c = if height c > height c' then c:c' : cs else c' : cs

main :: IO ()
main = loadInput >>= print . length . visible . parseGrid . lines
