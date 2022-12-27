module Day23.Common (move, parseElves) where
import           Data.Bifunctor (bimap)
import qualified Data.Map       as Map
import           Data.Maybe     (catMaybes, fromJust, isJust, mapMaybe)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Utils.Grid     (Coordinate)
import           Utils.Lists    (maybeHead)

adjacents :: Coordinate -> Set Coordinate
adjacents (x, y) = Set.fromList [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

directions :: Int -> [Coordinate]
directions start = [[(-1, -1), (0, -1), (1, -1)], [(1, 1), (0, 1), (-1, 1)], [(-1, 1), (-1, 0), (-1, -1)], [(1, -1), (1, 0), (1, 1)]] !! (start `mod` 4)

proposal :: Int -> Set Coordinate -> Coordinate -> (Coordinate, Maybe Coordinate)
proposal dir elves (x, y) = if Set.null $ Set.intersection elves (adjacents (x, y))
    then ((x, y), Nothing)
    else ((x, y), propose target) where
        available i = (all (\d -> Set.notMember (bimap (x +) (y +) d) elves) (directions (dir + i)), directions (dir + i) !! 1)
        target = maybeHead $ map snd $ filter fst $ map available [0..3]

        propose Nothing         = Nothing
        propose (Just (tx, ty)) = Just (x + tx, y + ty)

move :: Set Coordinate -> Int -> (Set Coordinate, Bool)
move elves dir = (Set.union removedOld $ Map.keysSet validProposals, Map.null validProposals) where
    proposals = Set.map (proposal dir elves) elves
    proposalsMap = Map.fromListWith (++) $ mapMaybe (\e -> if isJust (snd e) then Just (fromJust (snd e), [fst e]) else Nothing) $ Set.elems proposals
    validProposals = Map.filter (\ps -> length ps == 1) proposalsMap
    removedOld = Set.difference elves $ Set.fromList (map head $ Map.elems validProposals)

parseElves :: String -> Set Coordinate
parseElves inp = Set.fromList [(x, y) | (y, row) <- zip [0..] (lines inp), (x, c) <- zip [0..] row, c == '#']
