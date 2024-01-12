--
-- EPITECH PROJECT, 2023
-- ast
-- File description:
-- ast
--

module Ast (
    Ast(..),
    getSymbol,
    getInteger,
    getList,
    printTree,
    cptToAST,
    cptListToAstList,
) where

import Data.Maybe
import Cpt

data Ast = AstInteger Int
    | AstSymbol String
    | AstBoolean String
    | AstCall [Ast]
    | AstDefine (Either String [String]) Ast
    | AstLambda [String] Ast deriving (Show, Eq)

getSymbol :: Cpt -> Maybe String
getSymbol (CptSymbols s) = Just s
getSymbol _  = Nothing

getInteger :: Cpt -> Maybe Int
getInteger (CptInt n) = Just n
getInteger _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (CptList lst) = Just lst
getList _  = Nothing

printTree :: Cpt -> Maybe String
printTree (CptInt n) = Just $ "a Number " ++ show n
printTree (CptSymbols s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (CptList []) = Nothing
printTree (CptList xs) = Just $ "a List with " ++ describeElements xs

describeElements :: [Cpt] -> String
describeElements [] = "no elements"
describeElements [x] = "1 element: " ++ show x
describeElements [x, y] = "2 elements: " ++ show x ++ " and " ++ show y
describeElements [x, y, z] = "3 elements: " ++ show x ++ ", " ++ show y ++ " and " ++ show z
describeElements (x:y:z:xs) = "3 elements: " ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ", " ++ maybeEmptyList xs

maybeEmptyList :: [Cpt] -> String
maybeEmptyList [] = ""
maybeEmptyList xs = "and " ++ show (length xs) ++ " more elements"

cptToAST :: Cpt -> Maybe Ast
cptToAST (CptInt n) = Just (AstInteger n)
cptToAST (CptSymbols s) = Just (AstSymbol s)
cptToAST (CptList []) = Nothing

cptToAST (CptList (CptSymbols "define" : CptSymbols var : exprs)) =
  case mapMaybe cptToAST exprs of
    [value] -> Just (AstDefine (Left var) value)
    _ -> Nothing

cptToAST (CptList (CptSymbols func : args)) =
  case mapMaybe cptToAST args of
    [] -> Nothing
    argList -> Just (AstCall (AstSymbol func : argList))

cptToAST (CptList (CptInt n : xs)) = Just (AstCall (AstInteger n : mapMaybe cptToAST xs))
cptToAST (CptList (CptSymbols s : xs)) = Just (AstCall (AstSymbol s : mapMaybe cptToAST xs))
cptToAST _ = Nothing

cptListToAstList :: [Cpt] -> Maybe [Ast]
cptListToAstList [] = Just []
cptListToAstList (x:xs) =
  case cptToAST x of
    Just ast ->
      case cptListToAstList xs of
        Just astList -> Just (ast : astList)
        Nothing -> Nothing
    Nothing -> Nothing
