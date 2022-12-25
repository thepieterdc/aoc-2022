module Day18.Advanced where

import           Data.List    (maximumBy, minimumBy)
import           Data.Ord     (comparing)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Day18.Common (Cube, adjacent, parseCube, x, y, z)
import           Utils.IO     (loadInput)

circumference :: [Cube] -> ((Int, Int), (Int, Int), (Int, Int))
circumference cubes = ((minX - 1, maxX + 2), (minY - 1, maxY + 2), (minZ - 1, maxZ + 2)) where
    minX = x $ minimumBy (comparing x) cubes
    maxX = x $ maximumBy (comparing x) cubes
    minY = y $ minimumBy (comparing y) cubes
    maxY = y $ maximumBy (comparing y) cubes
    minZ = z $ minimumBy (comparing z) cubes
    maxZ = z $ maximumBy (comparing z) cubes

floodFill :: Set Cube -> ((Int, Int), (Int, Int), (Int, Int)) -> [Cube] -> Set Cube -> [Cube]
floodFill _ _ [] _ = []
floodFill cubes cf@((minX, maxX), (minY, maxY), (minZ, maxZ)) (c:cs) seen = if Set.member c seen
    then floodFill cubes cf cs seen
    else waters ++ floodFill cubes cf nextCs (Set.insert c seen) where
        nbs = Set.filter (\c' -> minX <= x c' && x c' <= maxX && minY <= y c' && y c' <= maxY && minZ <= z c' && z c' <= maxZ) $ adjacent c
        waters = Set.elems $ Set.intersection nbs cubes
        nextCs = Set.elems $ Set.union (Set.fromList cs) $ Set.difference nbs cubes

waterArea :: [Cube] -> [Cube]
waterArea cubes = floodFill (Set.fromList cubes) cf [(minX, minY, minZ)] Set.empty where
    cf@((minX, maxX), (minY, maxY), (minZ, maxZ)) = circumference cubes

main :: IO ()
main = loadInput >>= print . length . waterArea . map parseCube . lines
