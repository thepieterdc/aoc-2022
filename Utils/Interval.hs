module Utils.Interval (module Utils.Interval) where

-- Returns True if the first interval fully contains the second one
contains :: Ord a => (a, a) -> (a, a) -> Bool
contains (a, b) (c, d) = a <= c && d <= b

containsValue :: Ord a => (a, a)-> a -> Bool
containsValue (start, end) val = start <= val && val <= end

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (a, b) (c, d) = (a >= c && a <= d) || (b >= c && b <= d) || (c >= a && c <= b) || (d <= a && d >= b)
