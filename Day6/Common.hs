module Day6.Common (run) where

import qualified Data.Set as Set (fromList, size)

find :: Int -> String -> Int
find _ "" = 1
find inc str = if Set.size (Set.fromList (take inc str)) == inc
    then 0
    else find inc (drop 1 str) + 1

run :: Int -> String -> Int
run = find
