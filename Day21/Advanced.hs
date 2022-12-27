module Day21.Advanced where
import           Data.List    (mapAccumL)
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Maybe   (fromJust)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Day21.Common (Operation (..), eval, operands, parse)
import           Prelude      hiding (reverse)
import           Utils.IO     (loadInput)

clear :: Map String Operation -> Map String Operation
clear d = replacedRoot where
    (Addition a b) = fromJust $ Map.lookup "root" d
    removedHumn = Map.delete "humn" d
    replacedA = case eval a removedHumn of
        (Just i) -> Map.insert a (Literal i) removedHumn
        _        -> removedHumn
    replacedB = case eval b replacedA of
        (Just i) -> Map.insert b (Literal i) replacedA
        _        -> replacedA
    replacedRoot = Map.adjust (const (Subtraction a b)) "root" replacedB

dependents :: String -> Set String -> [(String, Operation)] -> [(String, Operation)]
dependents _ _ [] = []
dependents target seen ((k, op):xs) = if Set.notMember k seen
    then case op of
        (Addition a b) -> ([(k, op) | a == target || b == target]) ++ dependents target seen xs
        (Division a b) -> ([(k, op) | a == target || b == target]) ++ dependents target seen xs
        (Multiply a b) -> ([(k, op) | a == target || b == target]) ++ dependents target seen xs
        (Subtraction a b) -> ([(k, op) | a == target || b == target]) ++ dependents target seen xs
        _ -> dependents target seen xs
    else dependents target seen xs

invert :: String -> (String, Operation) -> Operation
invert target (key, Addition a b) = Subtraction key (if target == b then a else b)
invert target (key, Division a b) = if target == b then Division a key else Multiply b key
invert target (key, Multiply a b) = Division key (if target == b then a else b)
invert target (key, Subtraction a b) = if target == b then Subtraction a key else Addition b key

reverse :: String -> Set String -> Map String Operation -> Map String Operation
reverse target seen d = foldl process d $ dependents target seen (Map.assocs d) where
    process mp (monkey, op) = reverse opR seen' $ reverse opL seen' mp' where
        invertedOp = invert target (monkey, op)
        mp' = Map.delete monkey $ Map.insert target invertedOp mp
        seen' = Set.insert target seen
        (opL, opR) = operands invertedOp

main :: IO ()
main = loadInput >>= print . fromJust . eval "humn" . Map.insert "root" (Literal 0) . reverse "humn" Set.empty . clear . parse
