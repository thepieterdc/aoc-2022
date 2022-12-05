module Day3.Simple (main) where

import           Data.List   (intersect)
import           Day3.Common (prioritise)
import           Utils.IO    (loadInput)

compartments :: String -> (String, String)
compartments line = splitAt half line
    where half = length line `div` 2

findDuplicate :: (String, String) -> Char
findDuplicate (left, right) = head (left `intersect` right)

main :: IO ()
main = do {loadInput >>= print . sum . map (prioritise . findDuplicate . compartments) . lines}
