module Day1.Advanced (main) where

import           Data.List   (sortOn)
import           Data.Ord    (Down (Down))
import           Day1.Common (run)
import           Utils.IO    (loadInput)

main :: IO ()
main = do {loadInput >>= print . sum . take 3 . sortOn Down . run}
