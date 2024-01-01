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
parseList (")" : xs) = ([], xs)
parseList tokens =
    let (cpt, xs) = parseElement tokens
        (cptList, rest) = parseList xs
    in (cpt : cptList, rest)

parseElement :: [String] -> (Cpt, [String])
parseElement (token : xs)
    | (head token == '-' && all isDigit (tail token)) || all isDigit token = (CptInt (read token), xs)
    | token == "(" =
        let (cptList, rest) = parseList xs
        in (CptList cptList, rest)
    | otherwise = (CptSymbols token, xs)
