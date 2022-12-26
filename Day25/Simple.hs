module Day25.Simple where

import           Day25.Common (run)
import           Utils.IO     (loadInput)

main :: IO ()
main = loadInput >>= print . run
