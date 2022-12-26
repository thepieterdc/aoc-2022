module Day18.Simple where

import qualified Data.Set     as Set
import           Day18.Common (Cube, freeSides, parseCube)
import           Utils.IO     (loadInput)

findSides :: [Cube] -> Int
findSides cubes = sum $ map ((6 - ). length . freeSides (Set.fromList cubes)) cubes

main :: IO ()
main = loadInput >>= print . findSides . map parseCube . lines
