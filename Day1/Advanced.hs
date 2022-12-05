module Day1.Advanced (main) where

import           Data.List       (reverse, sortOn)
import           Data.Ord        (Down (Down))
import           Utils.Filtering (isNumber)
import           Utils.IO        (loadInput)
import           Utils.Parsing   (parseInt)


calculateSums :: Int -> [String] -> [Int]
calculateSums result [] = [result]
calculateSums c (x:xs) = if isNumber x
    then calculateSums (parseInt x + c) xs
    else c : calculateSums 0 xs

main :: IO ()
main = do {loadInput >>= print . sum . take 3 . sortOn Down . calculateSums 0 . lines}
