module Day7.Simple (main) where

import           Day7.Common (Tree (Dir, File), run, size)
import           Utils.IO    (loadInput)

findDirectorySizes :: Tree -> [Int]
findDirectorySizes d@(Dir _ fs) = size d : concatMap findDirectorySizes fs
-- we only care about directories.
findDirectorySizes _            = [0]

main :: IO ()
main = do {loadInput >>= putStrLn . show . run}
