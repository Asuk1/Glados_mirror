--
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Eval
--

module Eval
    (
        add,
        sub,
        Result (..),
    ) where

import Lib

data Result = Value Int | Bool String | Err String

instance Eq Result where
    (Value x) == (Value y) = x == y
    (Bool s1) == (Bool s2) = s1 == s2
    (Err e1) == (Err e2) = e1 == e2
    _ == _ = False

instance Show Result where
    show (Value x) = "Value " ++ show x
    show (Bool s) = "Bool " ++ show s
    show (Err s) = "Err " ++ show s




getValue :: Ast -> Env -> Result
getValue (AstInteger a) _ = Value a
getValue (AstBoolean a) _ = Bool a
getValue (AstSymbol s) env =
    case lookup s env of
        Just value -> getValue value env
        Nothing -> Err ("Symbol '" ++ s ++ "' not found in the environment.")
getValue _ _ = Err "Error: Unsupported expression type"



add :: [Ast] -> Env -> Result
add [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x + y)
        (Err errA, _) -> Err ("Error in 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in 'b': " ++ errB)
        _ -> Err "Error: Addition requires two integer values."
add _ _ = Err "Error in add: Insufficient arguments"


sub :: [Ast] -> Env -> Result
sub [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x - y)
        (Err errA, _) -> Err ("Error in 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in 'b': " ++ errB)
        _ -> Err "Error: Subtract requires two integer values."
sub _ _ = Err "Error in sub: Insufficient arguments"


