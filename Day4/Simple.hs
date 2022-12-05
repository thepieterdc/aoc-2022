module Day4.Simple (main) where

import           Day4.Common    (run)
import           Utils.Interval (contains)
import           Utils.IO       (loadInput)

_contains :: (Int, Int) -> (Int, Int) -> Bool
_contains fst snd = contains fst snd || contains snd fst

main :: IO ()
main = do {loadInput >>= print . run (uncurry _contains)}
