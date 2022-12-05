module Utils.Filtering (module Utils.Filtering) where
import           Data.Char (isDigit)

-- |Counts the amount of items in the list that match the given predicate.
countWhere :: (a -> Bool) -> [a] -> Int
countWhere fn items = length $ filter fn items

isNumber :: String -> Bool
isNumber "" = False
isNumber a  = all isDigit a
