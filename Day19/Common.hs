module Day19.Common (Blueprint, Robot(..), parse) where

import           Utils.Parser (Parser, digits, doParse, integer, many, string,
                               token)

-- ore cost (ore) | clay cost (ore) | obsid cost (ore/clay) | geode cost (ore/obsidian)
type Blueprint = (Int, Int, (Int, Int), (Int, Int))

data Robot = OreRobot Int Int
           | ClayRobot Int Int
           | ObsRobot Int Int
           | GeodeRobot Int Int deriving (Eq, Show)

parse' :: Parser Blueprint
parse' = do
    string "Blueprint "
    digits
    string ": Each ore robot costs "
    oreCost <- integer
    string " ore. Each clay robot costs "
    clayCost <- integer
    string " ore. Each obsidian robot costs "
    obsOreCost <- integer
    string " ore and "
    obsClayCost <- integer
    string " clay. Each geode robot costs "
    geodeOreCost <- integer
    string " ore and "
    geodeObsCost <- integer
    string " obsidian.\n"
    return (oreCost, clayCost, (obsOreCost, obsClayCost), (geodeOreCost, geodeObsCost))

parse :: String -> [Blueprint]
parse = doParse (many parse')
