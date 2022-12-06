module Day6.Simple (main) where
import           Day6.Common (run)
import           Utils.IO    (loadInput)

--
-- The rows are transposed to better facilitate operations.
--
main :: IO ()
main = do {loadInput >>= print . (+4) . run 4}
