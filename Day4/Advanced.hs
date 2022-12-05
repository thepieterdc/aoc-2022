module Day4.Advanced (main) where

import           Day4.Common    (run)
import           Utils.Interval (overlaps)
import           Utils.IO       (loadInput)

main :: IO ()
main = do {loadInput >>= print . run (uncurry overlaps)}
