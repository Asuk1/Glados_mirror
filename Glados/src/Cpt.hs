--
-- EPITECH PROJECT, 2024
-- Glados_mirror
-- File description:
-- Cpt
--

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
    (_, rest) -> error $ "Unexpected tokens remaining: " ++ show rest

parseList :: [String] -> ([Cpt], [String])
parseList [] = ([], [])
parseList (")" : xs) = ([], xs)
parseList tokens =
    let (cpt, xs) = parseElement tokens
        (cptList, rest) = parseList xs
    in (cpt : cptList, rest)

parseElement :: [String] -> (Cpt, [String])
parseElement [] = error "parseElement called with an empty list"
parseElement (token : xs)
    | all isDigit token = (CptInt (read token), xs)
    | token == "-" = (CptSymbols "-", xs)
    | head token == '-' && all isDigit (tail token) = (CptInt (read token), xs)
    | token == "(" =
        let (cptList, rest) = parseList xs
        in (CptList cptList, rest)
    | otherwise = (CptSymbols token, xs)
