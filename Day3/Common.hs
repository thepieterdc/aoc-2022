module Day3.Common (prioritise) where
import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)

prioritise :: Char -> Int
prioritise c = fromJust (elemIndex c (['a'..'z'] ++ ['A'..'Z'])) + 1
