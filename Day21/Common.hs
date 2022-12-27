module Day21.Common (Operation(..), eval, operands, parse) where

import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Prelude      hiding (until)
import           Utils.IO     (loadInput)
import           Utils.Parser (Parser, char, doParse, integer, string, token,
                               until, void, (<|>))

data Operation = Addition String String
               | Division String String
               | Literal Int
               | Multiply String String
               | Subtraction String String deriving (Eq, Show)

eval :: String -> Map String Operation -> Maybe Int
eval k d = case Map.lookup k d of
    (Just (Addition a b))    ->  case (eval a d, eval b d) of
        (Just a', Just b') -> Just (a' + b')
        _                  -> Nothing
    (Just (Division a b))    ->  case (eval a d, eval b d) of
        (Just a', Just b') -> Just (a' `div` b')
        _                  -> Nothing
    (Just (Literal i))       -> Just i
    (Just (Multiply a b))    ->  case (eval a d, eval b d) of
        (Just a', Just b') -> Just (a' * b')
        _                  -> Nothing
    (Just (Subtraction a b)) -> case (eval a d, eval b d) of
        (Just a', Just b') -> Just (a' - b')
        _                  -> Nothing
    _                        -> Nothing

operands :: Operation -> (String, String)
operands (Addition a b)    = (a, b)
operands (Division a b)    = (a, b)
operands (Multiply a b)    = (a, b)
operands (Subtraction a b) = (a, b)

parse :: String -> Map String Operation
parse = Map.fromList . map (doParse parse') . lines

parse' :: Parser (String, Operation)
parse' = do {n <- until ':'; void char; op <- parseDiv <|> parseLiteral <|> parseMin <|> parseMul <|> parsePlus; return (init n, op)} where
    operand = do {a <- char; b <- char; c <- char; d <- char; return $ a : b : c : [d]}
    parseDiv = do {a <- operand; string " / "; Division a <$> operand}
    parseLiteral = do Literal <$> integer
    parseMin = do {a <- operand; string " - "; Subtraction a <$> operand}
    parseMul = do {a <- operand; string " * "; Multiply a <$> operand}
    parsePlus = do {a <- operand; string " + "; Addition a <$> operand}
