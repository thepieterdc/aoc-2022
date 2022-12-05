module Day1.Simple (main) where

import           Day1.Common (run)
import           Utils.IO    (loadInput)

main :: IO ()
main = do {loadInput >>= print . maximum . run}
