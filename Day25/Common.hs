module Day25.Common (run) where

fromSnafu :: String -> Int
fromSnafu inp = fromSnafu' 0 (5 ^ length inp) inp where
    fromSnafu' ret _ [] = ret
    fromSnafu' ret pw (c:cs) = case c of
        '1' -> fromSnafu' (ret + (pw `div` 5)) (pw `div` 5) cs
        '2' -> fromSnafu' (ret + ((pw `div` 5) * 2)) (pw `div` 5) cs
        '-' -> fromSnafu' (ret - (pw `div` 5)) (pw `div` 5) cs
        '=' -> fromSnafu' (ret - ((pw `div` 5) * 2)) (pw `div` 5) cs
        _   -> fromSnafu' ret (pw `div` 5) cs

toSnafu :: Int -> String
toSnafu 0 = ""
toSnafu inp = case inp `mod` 5 of
    0 -> toSnafu (inp `div` 5) ++ "0"
    1 -> toSnafu (inp `div` 5) ++ "1"
    2 -> toSnafu (inp `div` 5) ++ "2"
    3 -> toSnafu (inp `div` 5 + 1) ++ "="
    4 -> toSnafu (inp `div` 5 + 1) ++ "-"
    _ -> ""

run :: String -> String
run = toSnafu . sum . map fromSnafu . lines
