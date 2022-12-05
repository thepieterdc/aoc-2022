module Utils.Parser (module Utils.Parser, many, some, (<|>), void) where

import           Control.Applicative (Alternative, empty, many, some, (<|>))
import           Control.Monad       (MonadPlus, ap, guard, liftM, mplus, mzero,
                                      void)
import           Data.Char           (isDigit)

newtype Parser a = Parser (String -> [(a, String)])

doParse :: (Show a) => Parser a -> String -> a
doParse m s = one [x | (x, t) <- apply m s, t == ""] where
    one [x] = x
    one [] = error ("Parse not completed:\n" ++ show s)
    one xs | length xs > 1 = error ("Multiple parses found:\n" ++ show xs)
    one _ = error "Unknown error"

apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

instance Alternative Parser where
    empty :: Parser a
    empty = mzero

    many :: Parser a -> Parser [a]
    many p = some p `mplus` return []

    some :: Parser a -> Parser [a]
    some p = do { x <- p; xs <- many p; return (x:xs)}

    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) p q = Parser (\s ->
                        case apply p s of
                            []  -> apply q s
                            res -> res)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> [(x, s)])

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) = ap

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = liftM

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    m >>= k = Parser (\s -> [(y, u) | (x, t) <- apply m s, (y, u) <- apply (k x) t])

instance MonadPlus Parser where
    mplus :: Parser a -> Parser a -> Parser a
    mplus m n = Parser (ap ((++) . apply m) (apply n))

    mzero :: Parser a
    mzero = Parser (const [])

char :: Parser Char
char = Parser f where
    f []    = []
    f (c:s) = [(c, s)]

chars :: Int -> Parser String
chars 0   = return ""
chars amt = do {x <- char; y <- chars (amt - 1); return (x : y)}

digit :: Parser Char
digit = spot isDigit

digits :: Parser String
digits = some digit

optional :: String -> Parser String
optional s = string s <|> return []

spot :: (Char -> Bool) -> Parser Char
spot p = do {c <- char; guard (p c); return c}

string :: String -> Parser String
string s = do {cs <- chars (length s); guard (cs == s); return cs}

token :: Char -> Parser ()
token c = void (spot (c == ))
