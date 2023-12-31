module Tokenization
    (
        stringToToken,
    ) where

stringToToken :: String -> [String]
stringToToken [] = []
stringToToken (x:xs)
  | x `elem` " \n" = stringToToken xs
  | x `elem` "()" = [x] : stringToToken xs
  | otherwise = (x : takeWhile (`notElem` " \t\n()") xs) : stringToToken (dropWhile (`notElem` " \t\n()") xs)

