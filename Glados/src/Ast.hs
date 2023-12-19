--
-- EPITECH PROJECT, 2023
-- ast
-- File description:
-- ast
--

module Ast (
    SExpr(..),
    getSymbol,
    getInteger,
    getList,
    printTree,
) where

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving Show

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _  = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInt n) = Just n
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList lst) = Just lst
getList _  = Nothing

printTree :: SExpr -> Maybe String
printTree (SInt n) = Just $ "a Number " ++ show n
printTree (SSymbol s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (SList []) = Nothing
printTree (SList xs) = Just $ "a List with " ++ describeElements xs

describeElements :: [SExpr] -> String
describeElements [] = "no elements"
describeElements [x] = "1 element: " ++ show x
describeElements [x, y] = "2 elements: " ++ show x ++ " and " ++ show y
describeElements [x, y, z] = "3 elements: " ++ show x ++ ", " ++ show y ++ " and " ++ show z
describeElements (x:y:z:xs) = "3 elements: " ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ", " ++ maybeEmptyList xs

maybeEmptyList :: [SExpr] -> String
maybeEmptyList [] = ""
maybeEmptyList xs = "and " ++ show (length xs) ++ " more elements"


