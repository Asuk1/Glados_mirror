--
-- EPITECH PROJECT, 2024
-- Glados_mirror
-- File description:
-- Tokenization
--

module Tokenization
    (
        stringToToken,
        checkParenthesis,
    ) where

stringToToken :: String -> [String]
stringToToken [] = []
stringToToken (x:xs)
  | x `elem` " \n" = stringToToken xs
  | x == ';' = if null (filter (\c -> c `notElem` " \n") xs) then [")"] else [")", "("] ++ stringToToken xs
  | x == '(' = "(" : stringToToken xs
  | x == ')' = ")" : stringToToken xs
  | otherwise = (x : takeWhile (`notElem` " \t\n();") xs) : stringToToken (dropWhile (`notElem` " \t\n();") xs)

checkParenthesis :: [String] -> (Int, Int)
checkParenthesis tokens = (countOpen, countClose)
  where
    x = filter (`elem` "()") (concat tokens)
    countOpen = length $ filter (== '(') x
    countClose = length $ filter (== ')') x
