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

import Data.Maybe

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving Show

data Ast = AstInteger Int
    | AstSymbol String
    | AstBoolean String
    | AstCall [Ast]
    | AstDefine (Either String [String]) Ast
    | AstLambda [String] Ast deriving (Show)

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

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt n) = Just (AstInteger n)
sexprToAST (SSymbol s) = Just (AstSymbol s)
sexprToAST (SList []) = Nothing

sexprToAST (SList (SSymbol "define" : SSymbol var : exprs)) =
  case mapMaybe sexprToAST exprs of
    [value] -> Just (AstDefine (Left var) value)
    _ -> Nothing

sexprToAST (SList (SSymbol func : args)) =
  case mapMaybe sexprToAST args of
    [] -> Nothing
    argList -> Just (AstCall (AstSymbol func : argList))

sexprToAST (SList (SInt n : xs)) = Just (AstCall (AstInteger n : mapMaybe sexprToAST xs))
sexprToAST (SList (SSymbol s : xs)) = Just (AstCall (AstSymbol s : mapMaybe sexprToAST xs))
sexprToAST _ = Nothing



