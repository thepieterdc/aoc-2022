module Day11.Advanced where

import           Day11.Common (Monkey, clearItems, incProcessed, items,
                               monkeyBusiness, parse, process, target, test,
                               transfer)
import           Utils.IO     (loadInput)
import           Utils.Lists  (mapIdx)

commonTest :: [Monkey] -> [(Int, Monkey)]
commonTest ms = map (product (map test ms),) ms

run :: Int -> [(Int, Monkey)] -> [(Int, Monkey)]
run idx divMonkeys = if idx >= length divMonkeys then divMonkeys else
    let divMonkey = divMonkeys !! idx
        (div, monkey) = divMonkey
        monkeys = map snd divMonkeys
        processedItems = map ((\i -> (target monkey i, i)) . (`mod` div) . process monkey) $ items monkey
        newMonkeys = foldl (uncurry . transfer) monkeys processedItems
    in run (idx + 1) (map (div,) (mapIdx idx (incProcessed (length processedItems) . clearItems) newMonkeys))

main :: IO ()
main = loadInput >>= print . monkeyBusiness . map snd . (!! 10000) . iterate (run 0) . commonTest . parse
