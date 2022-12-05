module Day1.Simple (main) where

import           Utils.Filtering (isNumber)
import           Utils.IO        (loadInput)
import           Utils.Parsing   (parseInt)

findMax :: Int -> Int -> [String] -> Int
findMax m c [] = max m c
findMax m c (x:xs) = if isNumber x
    then findMax m (c + parseInt x) xs
    else findMax (max m c) 0 xs


main :: IO ()
main = do {loadInput >>= print . findMax 0 0 . lines}
