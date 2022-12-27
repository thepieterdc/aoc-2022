module Day23.Simple where

import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Day23.Common (move, parseElves)
import           Utils.Grid   (Coordinate)
import           Utils.IO     (loadInput)

run :: Int -> Set Coordinate -> Int
run d elves = case move elves d of
    (_, True)   -> 1
    (elves', _) -> 1 + run (d + 1) elves'

main :: IO ()
main = loadInput >>= print . run 0 . parseElves
