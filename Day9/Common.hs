module Day9.Common (run) where
import           Data.Bifunctor (bimap)
import           Data.Foldable  (foldl')
import           Data.List      (mapAccumL, nub)
import           Utils.Grid     (Coordinate, chebyshev, orthogonal)

--
-- Types
--

-- |Describes the coordinate transformation
type Direction = (Int, Int)

--
-- Parsing
--

parse :: String -> [Coordinate]
parse = concatMap parseMove . lines

parseMove :: String -> [Coordinate]
parseMove mv = replicate (read (drop 2 mv) :: Int) $ parseDirection $ head mv where
        parseDirection 'D' = (0, -1)
        parseDirection 'L' = (-1, 0)
        parseDirection 'R' = (1, 0)
        parseDirection 'U' = (0, 1)
        parseDirection _   = error "Invalid direction"

--
-- Utils
--

follow :: Coordinate -> Coordinate -> Coordinate
follow t@(tx, ty) h@(hx, hy) = case (chebyshev t h <= 1, (hx - tx, hy - ty)) of
    -- Don't need to move.
    (True, _)     -> t
    -- Move along X axis.
    (_, (_, 0))   -> ((hx + tx) `div` 2, ty)
    -- Move along Y axis.
    (_, (0, _))   -> (tx, (hy + ty) `div` 2)
    -- Move diagonally to down-left.
    (_, (-1, -2)) -> (tx - 1, ty - 1)
    (_, (-2, -2)) -> (tx - 1, ty - 1)
    (_, (-2, -1)) -> (tx - 1, ty - 1)
    -- Move diagonally to down-right.
    (_, (1, -2))  -> (tx + 1, ty - 1)
    (_, (2, -2))  -> (tx + 1, ty - 1)
    (_, (2, -1))  -> (tx + 1, ty - 1)
    -- Move diagonally to up-left.
    (_, (-2, 2))  -> (tx - 1, ty + 1)
    (_, (-2, 1))  -> (tx - 1, ty + 1)
    (_, (-1, 2))  -> (tx - 1, ty + 1)
    -- Move diagonally to up-right.
    (_, (1, 2))   -> (tx + 1, ty + 1)
    (_, (2, 1))   -> (tx + 1, ty + 1)
    (_, (2, 2))   -> (tx + 1, ty + 1)
    (_, diff)     -> error $ show diff

moveHead :: [Direction] -> [Coordinate]
moveHead mvs = (0, 0) : map snd (snd $ mapAccumL mv (0, 0) mvs) where
    mv c d = (bimap (fst c +) (snd c +) d, (d, bimap (fst c +) (snd c +) d))

trail :: Int -> [Coordinate] -> [Coordinate]
trail amt cs = iterate (foldl' (\r c -> r ++ [follow (last r) c]) [(0, 0)]) cs !! amt

run :: Int -> String -> Int
run amt = length . nub . trail amt . moveHead . parse
