{-|
Module      : Utils.Lists
Description : Contains methods to operate on lists.
Copyright   : (c) Pieter De Clercq, 2022
License     : MIT

Contains methods to operate on lists.
-}
module Utils.Lists (module Utils.Lists) where

-- |Applies the given function to the element at the given position in the list.
mapIdx ::
    Int -- ^ The position in the list to map
    -> (a -> a) -- ^ The mapping function to execute
    -> [a] -- ^ The input list
    -> [a] -- ^ The resulting list
mapIdx pos f l = take pos l ++ f (l !! pos) : drop (pos + 1) l
