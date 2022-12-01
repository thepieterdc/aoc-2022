module Utils.Filtering (module Utils.Filtering) where
import           Data.Char (isDigit)

isNumber :: String -> Bool
isNumber "" = False
isNumber a  = all isDigit a
