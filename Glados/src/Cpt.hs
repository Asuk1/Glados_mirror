module Cpt (
    Cpt(..),
    tokenToCpt,
    parseList,
    parseElement,
) where

import Data.Char (isDigit)

data Cpt = CptList [Cpt] | CptSymbols String | CptInt Int
    deriving (Show, Eq)

tokenToCpt :: [String] -> [Cpt]
tokenToCpt tokens = case parseList tokens of
    (cpts, []) -> cpts

parseList :: [String] -> ([Cpt], [String])
parseList [] = ([], [])
parseList (")" : rest) = ([], rest)
parseList tokens =
    let (cpt, rest1) = parseElement tokens
        (subList, rest2) = parseList rest1
    in (cpt : subList, rest2)

parseElement :: [String] -> (Cpt, [String])
parseElement (token : rest)
    | (head token == '-' && all isDigit (tail token)) || all isDigit token = (CptInt (read token), rest)
    | token == "(" =
        let (subList, restList) = parseList rest
        in (CptList subList, restList)
    | otherwise = (CptSymbols token, rest)
