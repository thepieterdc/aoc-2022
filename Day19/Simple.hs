module Day19.Simple where

import           Day19.Common (Blueprint, Robot (..), parse)
import           Utils.IO     (loadInput)

process :: [Robot] -> Int -> Blueprint -> Int
process _ 0 _ = 0
process _ a _ = a

main :: IO ()
main = loadInput >>= print . map (process [OreRobot 0 1, ClayRobot 0 0, ObsRobot 0 0, GeodeRobot 0 0] 24) . take 1 . parse
