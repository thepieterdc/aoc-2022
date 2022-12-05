module Utils.Lists (module Utils.Lists) where

-- |Applies the given function to the element at the given position in the list.
mapIdx :: Int -> (a -> a) -> [a] -> [a]
mapIdx pos f l = take pos l ++ f (l !! pos) : drop (pos + 1) l
