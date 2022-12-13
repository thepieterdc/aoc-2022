module Day12.Simple where

import           Data.Bifunctor (second)
import           Data.Char      (chr, ord)
import           Data.List      (groupBy, minimumBy)
import           Data.Maybe     (fromMaybe)
import           Utils.Grid     (Coordinate, manhattan)
import           Utils.IO       (loadInput)
import           Utils.Lists    (mapIdx)

--
-- Types
--

-- Coordinate, Elevation, IsStart, IsEnd
data Cell = Cell Coordinate Int Bool Bool deriving (Show)

-- Cell, parentIdx, open, closed, g, h, f
data Node = Node Int Cell (Maybe Int) Bool Bool Int Int Int deriving (Show)

-- amount cols, amount rows, nodes
data Grid = Grid Int Int [Node] deriving (Show)

--
-- Parsing
--

parse :: [String] -> Grid
parse ls = Grid (length $ head ls) (length ls) nodes where
    elevation 'S' = 0
    elevation 'E' = 25
    elevation e   = ord e - 97

    start (Cell _ _ True _) = True
    start _                 = False

    cells = [Cell (x, y) (elevation c) (c == 'S') (c == 'E') | (y, r) <- zip [0..] ls, (x, c) <- zip [0..] r]
    nodes = zipWith (curry (\ c -> uncurry Node c Nothing (start (snd c)) False 0 0 0)) [0..] cells

--
-- Utils
--

coordinates :: Node -> Coordinate
coordinates (Node _ (Cell c _ _ _) _ _ _ _ _ _) = c

elevation :: Node -> Int
elevation (Node _ (Cell _ e _ _) _ _ _ _ _ _) = e

idx :: Node -> Int
idx (Node i _ _ _ _ _ _ _) = i

isClosed :: Node -> Bool
isClosed (Node _ _ _ _ c _ _ _) = c

isEnd :: Node -> Bool
isEnd (Node _ (Cell _ _ _ True) _ _ _ _ _ _) = True
isEnd _                                      = False

isOpen :: Node -> Bool
isOpen (Node _ _ _ o _ _ _ _) = o

-- isStartN :: Node -> Bool
-- isStartN (Node (Cell _ _ True _) _ _ _ _ _ _) = True
-- isStartN _                                    = False

neighbours :: Grid -> Node -> [Node]
neighbours (Grid cols rows nodes) (Node i (Cell _ e _ _) _ _ _ _ _ _) = nbs where
    -- top, left, right, down
    ids = [i - cols, i - 1, i + 1, i + cols]
    nbs = filter (\n -> (elevation n - e) <= 1) [nodes !! i | i <- ids, 0 < i && i < length nodes]

-- parentIdx :: Node -> Maybe Int
-- parentIdx (Node _ p _ _ _ _ _) = p

pathfind :: Grid -> Grid
-- Mark the start node as open.
pathfind g@(Grid _ _ nodes) = pathfind' 1400 (coordinates end) g where
    cost (Node _ _ _ _ _ _ _ f) = f

    end = head $ filter isEnd nodes

    endFound n = isOpen $ n !! idx end

    close (Node i c p _ _ g h f) = Node i c p False True g h f

    updateNeighbour parent@(Node _ _ _ _ _ parentG _ _) end node@(Node i c@(Cell _ e _ _) _ _ _ g _ _) = newNode where
        newG = parentG + 1
        elevationRemaining = 25 - elevation node
        d = manhattan end (coordinates node)
        newH = sum $ zipWith (curry product) [1..d] (cycle (reverse [1..elevationRemaining]))
        newNode = Node i c (Just (idx parent)) True False newG newH (newG + newH)

    pathfind' :: Int -> Coordinate -> Grid -> Grid
    pathfind' stop end g@(Grid cols rows nodes) = if stop < 1 || endFound nodes then g else
        let current = minimumBy (\a b -> (if cost a < cost b then LT else GT)) $ filter isOpen nodes
            -- Ignore already closed nodes, process the neighbours.
            nbs = filter (not. isClosed) (neighbours g current)
            updatedNbs = map (updateNeighbour current end) $ filter (not. isClosed) $ neighbours g current
            -- Store the updates, mark the current node as closed
            updatedCurrent = mapIdx (idx current) close nodes
        in pathfind' (stop - 1) end (Grid cols rows (foldl (\ns n -> mapIdx (idx n) (const n) ns) updatedCurrent updatedNbs))

-- backtrack :: Int -> [(Int, Node)] -> [Coordinate]
-- backtrack currentIdx nodes = if isStartN here then [] else coordinates here : backtrack parent nodes where
--     here = snd (nodes !! currentIdx)
--     done = isStartN here
--     parent = fromMaybe 0 $ parentIdx here

fmt :: Grid -> [String]
fmt g@(Grid _ _ nodes) = map (map code) groups where
    groups = groupBy (\c1 c2 -> snd (coordinates c1) == snd (coordinates c2)) nodes
    code (Node _ (Cell _ _ True _) _ _ _ _ _ _) = 'S'
    code (Node _ (Cell _ _ _ True) _ _ _ _ _ _) = 'E'
    code (Node _ (Cell _ c _ _) _ True _ _ _ _) = chr (c + 65)
    code (Node _ _ _ _ True _ _ _)              = ' '
    code (Node _ (Cell _ c _ _) _ _ _ _ _ _)    = chr (c + 97)

main :: IO ()
-- main = loadInput >>= mapM_ putStrLn . fmt . pathfind . parse . lines
main = loadInput >>= mapM_ putStrLn . fmt . pathfind . parse . lines
-- main = loadInput >>= print . fmt . pathfind . parse . lines
