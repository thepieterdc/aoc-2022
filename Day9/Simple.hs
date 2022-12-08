module Day9.Simple where

import           Day9.Common (run)
import           Utils.IO    (loadInput)

main :: IO ()
main = loadInput >>= print . run 1
