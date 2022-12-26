module Day18.Common (Cube, adjacent, freeSides, parseCube, x, y, z) where

import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Data.Text (pack, singleton, splitOn, unpack)

type Cube = (Int, Int, Int)

adjacent :: Cube -> Set Cube
adjacent (x, y, z) = Set.fromList [
    (x, y, z + 1), -- back
    (x, y - 1, z), -- down
    (x, y, z - 1), -- front
    (x - 1, y, z), -- left
    (x + 1, y, z), -- right
    (x, y + 1, z)] -- up

freeSides :: Set Cube -> Cube -> Set Cube
freeSides cubes cube = Set.intersection cubes (adjacent cube)

parseCube :: String -> Cube
parseCube line = (read x :: Int, read y :: Int, read z :: Int) where
    (x:y:z:_) = map unpack . splitOn (singleton ',') $ pack line

x :: Cube -> Int
x (x', _, _) = x'

y :: Cube -> Int
y (_, y', _) = y'

z :: Cube -> Int
z (_, _, z') = z'
