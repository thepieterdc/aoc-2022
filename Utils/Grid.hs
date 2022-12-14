{-|
Module      : Utils.Grid
Description : Contains methods to handle working with grids.
Copyright   : (c) Pieter De Clercq, 2022
License     : MIT

Contains methods to handle working with grids.
-}
module Utils.Grid (module Utils.Grid) where

-- |A coordinate defined as a pair of (x, y)
type Coordinate = (Int, Int)

-- |Calculates the Chebyshev distance between two coordinates.
chebyshev :: Coordinate -> Coordinate -> Int
chebyshev (x1, y1) (x2, y2) = maximum $ map abs [x1 - x2, y1 - y2]

-- |Gets whether two coordinates are orthogonally placed.
--  If both coordinates are equal, they are considered orthogonal.
orthogonal :: Coordinate -> Coordinate -> Bool
orthogonal (ax, ay) (bx, by) = ax == bx || ay == by
