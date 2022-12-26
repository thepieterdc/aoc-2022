module Day21.Simple where
import           Data.Maybe   (fromJust)
import           Day21.Common (eval, parse)
import           Utils.IO     (loadInput)

main :: IO ()
main = loadInput >>= print . fromJust . eval "root" . parse
