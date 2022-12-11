module Day11.Advanced where

import           Day11.Common (parse, run, test)
import           Utils.IO     (loadInput)

main :: IO ()
main = loadInput >>= print . (\ms -> run (`mod` product (map test ms)) 10000 ms) . parse
