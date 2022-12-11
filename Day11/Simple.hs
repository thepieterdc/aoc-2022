module Day11.Simple where

import           Day11.Common (parse)
import           Utils.IO     (loadInput)

main :: IO ()
main = loadInput >>= print . parse
