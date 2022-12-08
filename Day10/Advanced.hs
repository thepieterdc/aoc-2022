module Day10.Advanced where

import           Data.List    (intercalate)
import           Day10.Common (run)
import           Utils.IO     (loadInput)
import           Utils.Lists  (groupBySize)
drawRow :: [Int] -> String
drawRow xs = map drawChar [0..(length xs - 1)] where
    drawChar idx = if (xs !! idx) `elem` [idx - 1, idx, idx + 1] then '#' else '.'

main :: IO ()
main = loadInput >>= mapM_ (putStrLn . drawRow) . groupBySize 40 . run
