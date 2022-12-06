module Day6.Advanced (main) where
import           Day6.Common (run)
import           Utils.IO    (loadInput)

main :: IO ()
main = do {loadInput >>= print . (+14) . run 14}
