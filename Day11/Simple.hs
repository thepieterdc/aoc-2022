module Day11.Simple where

import           Day11.Common (parse, run)
import           Utils.IO     (loadInput)

main :: IO ()
main = loadInput >>= print . run (`quot` 3) 20 . parse
