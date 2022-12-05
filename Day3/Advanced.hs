module Day3.Advanced (main) where

import           Data.List   (intersect)
import           Day3.Common (prioritise)
import           Utils.IO    (loadInput)

findBadges :: [String] -> [Char]
findBadges [] = []
findBadges lines = head (foldl1 intersect search):findBadges rest
    where (search,rest) = splitAt 3 lines

main :: IO ()
main = do {loadInput >>= print . sum . map prioritise . findBadges . lines}
