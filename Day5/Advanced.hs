module Day5.Advanced (main) where
import           Day5.Common (run)
import           Utils.IO    (loadInput)

--
-- The rows are transposed to better facilitate operations.
--
main :: IO ()
main = do {loadInput >>= print . run id}
