module Day10.Simple where

import           Day10.Common (run)
import           Utils.IO     (loadInput)

strengths :: [Int] -> [Int] -> [Int]
strengths positions xs = map strength positions where
    strength pos = pos * (xs !! (pos - 1))

main :: IO ()
main = loadInput >>= print . sum . strengths [20, 60, 100, 140, 180, 220] . run
