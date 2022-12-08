module Day7.Simple (main) where

import           Day7.Common (findDirectorySizes, run)
import           Utils.IO    (loadInput)

main :: IO ()
main = do {loadInput >>= print . sum . filter (<= 100000). findDirectorySizes . run}
