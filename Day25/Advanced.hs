module Day25.Advanced where

import           Day25.Common (run)
import           Utils.IO     (loadInput)

main :: IO ()
main = loadInput >>= print . run
