module Day12.Simple where

import           Data.Bifunctor (second)
import           Data.Char      (ord)
import           Data.List      (minimumBy)
import           Data.Maybe     (fromMaybe)
import           GHC.Real       (infinity)
import           Utils.Grid     (Coordinate, manhattan)
import           Utils.IO       (loadInput)
import           Utils.Lists    (mapIdx)

-- Coordinate, Elevation, IsStart, IsEnd
data Cell = Cell Coordinate Int Bool Bool deriving (Eq, Show)

-- Cell, parentIdx, open, closed, g, h, f
data Node = Node Cell (Maybe Int) Bool Bool Int Int Int deriving (Eq, Show)

coordinates :: Node -> Coordinate
coordinates (Node (Cell c _ _ _) _ _ _ _ _ _) = c

cost :: Node -> Int
cost (Node _ _ _ _ _ _ f) = f

distance :: Node -> Node -> Int
distance (Node (Cell c1 _ _ _) _ _ _ _ _ _) (Node (Cell c2 _ _ _) _ _ _ _ _ _) = manhattan c1 c2

endFound :: [(Int, Node)] -> Bool
endFound = any (\n -> isEnd (snd n) && isOpen (snd n))

elevation :: Node -> Int
elevation (Node (Cell _ e _ _) _ _ _ _ _ _) = e

isClosed :: Node -> Bool
isClosed (Node _ _ _ c _ _ _) = c

isEnd :: Node -> Bool
isEnd (Node (Cell _ _ _ True) _ _ _ _ _ _) = True
isEnd _                                    = False

isOpen :: Node -> Bool
isOpen (Node _ _ o _ _ _ _) = o

isStart :: Cell -> Bool
isStart (Cell _ _ True _) = True
isStart _                 = False

isStartN :: Node -> Bool
isStartN (Node (Cell _ _ True _) _ _ _ _ _ _) = True
isStartN _                                    = False

markClosed :: Node -> Node
markClosed (Node c p _ _ g h f) = Node c p False True g h f

markOpen :: Node -> Node
markOpen (Node c p _ _ g h f) = Node c p True False g h f

parentIdx :: Node -> Maybe Int
parentIdx (Node _ p _ _ _ _ _) = p

parse :: [String] -> [Cell]
parse ls = [Cell (x, y) (elevation c) (c == 'S') (c == 'E') | (y, r) <- zip [0..] ls, (x, c) <- zip [0..] r] where
    elevation 'S' = 0
    elevation 'E' = 25
    elevation e   = ord e - 97

pathfind :: [Cell] -> [Coordinate]
-- Mark the start node as open.
pathfind cells = backtrack endIdx $ pathfind' (coordinates endNode) nodeIds where
    nodes = map (\c -> Node c Nothing (isStart c) False 0 0 0) cells
    nodeIds = zip [0..] nodes
    coordinates (Node (Cell c _ _ _) _ _ _ _ _ _) = c
    (endIdx, endNode) = head $ filter (isEnd . snd) nodeIds

backtrack :: Int -> [(Int, Node)] -> [Coordinate]
backtrack currentIdx nodes = if isStartN here then [] else coordinates here : backtrack parent nodes where
    here = snd (nodes !! currentIdx)
    done = isStartN here
    parent = fromMaybe 0 $ parentIdx here

pathfind' :: Coordinate -> [(Int, Node)] -> [(Int, Node)]
pathfind' end nodes = if endFound nodes then nodes else
    let (currentIdx, current) = minimumBy (\a b -> (if cost (snd a) < cost (snd b) then LT else GT)) $ filter (isOpen . snd) nodes
        -- Ignore already closed nodes.
        nbs = filter (\n -> not (isClosed (snd n)) && (manhattan (coordinates current) (coordinates $ snd n) == 1) && (abs (elevation (snd n) - elevation current) <= 1)) nodes
        updatedNbs = map (updateNeighbour (currentIdx, current) end nodes) nbs
        -- Store the updates, mark the current node as closed
        updatedCurrent = mapIdx currentIdx (second markClosed) nodes
    in pathfind' end (foldl updateNode updatedCurrent updatedNbs)

updateNode :: [(Int, Node)] -> (Int, Node) -> [(Int, Node)]
updateNode nodes (idx, node) = mapIdx idx (const (idx, node)) nodes

updateNeighbour :: (Int, Node) -> Coordinate -> [(Int, Node)] -> (Int, Node) -> (Int, Node)
updateNeighbour (parentIdx, parentNode) end nodes (nodeIdx, node@(Node c _ _ _ _ _ _)) = (nodeIdx, newNode) where
    g (Node _ _ _ _ g' _ _) = g'
    newG = g parentNode + 1
    newH = manhattan end (coordinates node)
    newNode = Node c (Just parentIdx) True False newG newH (newG + newH)

main :: IO ()
main = loadInput >>= print . length . pathfind . parse . lines
