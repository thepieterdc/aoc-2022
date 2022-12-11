module Day11.Simple where

import           Day11.Common (Monkey, clearItems, incProcessed, items,
                               monkeyBusiness, parse, process, target, transfer)
import           Utils.IO     (loadInput)
import           Utils.Lists  (mapIdx)

run :: Int -> [Monkey] -> [Monkey]
run idx monkeys = if idx >= length monkeys then monkeys else
    let monkey = monkeys !! idx
        processedItems = map ((\i -> (target monkey i, i)) . (`quot` 3) . process monkey) $ items monkey
        newMonkeys = foldl (uncurry . transfer) monkeys processedItems
    in run (idx + 1) (mapIdx idx (incProcessed (length processedItems) . clearItems) newMonkeys)

main :: IO ()
main = loadInput >>= print . monkeyBusiness . (!! 20) . iterate (run 0) . parse
