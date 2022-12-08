module Day7.Advanced (main) where

import           Day7.Common (findDirectorySizes, run)
import           Utils.IO    (loadInput)

findDelete :: Int -> [Int] -> Int
findDelete target xs = minimum (filter (>= target) xs)

findRequiredSpace :: [Int] -> (Int, [Int])
findRequiredSpace is = (30000000 - (70000000 - head is), tail is)

main :: IO ()
main = do{loadInput >>= print . uncurry findDelete . findRequiredSpace . findDirectorySizes . run}
