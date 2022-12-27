module Day23.Simple where

import           Data.Foldable (maximumBy, minimumBy)
import           Data.Ord      (comparing)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Day23.Common  (move, parseElves)
import           Utils.Grid    (Coordinate)
import           Utils.IO      (loadInput)

eval :: Set Coordinate -> Int
eval elves = length [(x, y) | x <- [minX..maxX], y <- [minY..maxY], Set.notMember (x, y) elves] where
    minX = fst $ minimumBy (comparing fst) elves
    maxX = fst $ maximumBy (comparing fst) elves
    minY = snd $ minimumBy (comparing snd) elves
    maxY = snd $ maximumBy (comparing snd) elves

run :: Set Coordinate -> Set Coordinate
run elves = foldl (\e d -> fst $ move e d) elves [0..9]

main :: IO ()
main = loadInput >>= print . eval . run . parseElves
