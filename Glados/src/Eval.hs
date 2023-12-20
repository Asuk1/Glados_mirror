--
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Eval
--

module Eval
    (
        add,
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
getValue (AstSymbol s) env =
    case lookup s env of
        Just value -> getValue value env
        Nothing -> Err ("Symbol '" ++ s ++ "' not found in the environment.")
getValue _ _ = Err "Error: Unsupported expression type"

add :: [Ast] -> Env -> Result
add [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x + y)
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err ("Error in 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in 'b': " ++ errB)
        _ -> Err "Error: Addition requires two integer values."
add _ _ = Err "Error in add: Insufficient arguments"

subtract :: [Ast] -> Env -> Result
subtract [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x - y)
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err ("Error in 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in 'b': " ++ errB)
        _ -> Err "Error: Subtraction requires two integer values."
subtract _ _ = Err "Error in subtract: Insufficient arguments"

divide :: [Ast] -> Env -> Result
divide [a, b] env =
    case (getValue a env, getValue b env) of
        (_, Value 0) -> Err "Error: Division by zero."
        (Value x, Value y) -> Value (x `div` y)
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err ("Error in 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in 'b': " ++ errB)
        _ -> Err "Error: Division requires two integer values."
divide _ _ = Err "Error in divide: Insufficient arguments"

multiply :: [Ast] -> Env -> Result
multiply [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x * y)
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err ("Error in 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in 'b': " ++ errB)
        _ -> Err "Error: Multiplication requires two integer values."
multiply _ _ = Err "Error in multiply: Insufficient arguments"

modulo :: [Ast] -> Env -> Result
modulo [a, b] env =
    case (getValue a env, getValue b env) of
        (_, Value 0) -> Err "Error: Division by zero (modulo)."
        (Value x, Value y) -> Value (x `mod` y)
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err ("Error in 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in 'b': " ++ errB)
        _ -> Err "Error: Modulo requires two integer values."
modulo _ _ = Err "Error in modulo: Insufficient arguments"

equal :: [Ast] -> Env -> Result
equal [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Bool (show (x == y))
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err $ "Error in 'a': " ++ errA
        (_, Err errB) -> Err $ "Error in 'b': " ++ errB
        _ -> Err "Error: Equality comparison requires two values."
equal _ _ = Err "Error in equal: Insufficient arguments"

lessThan :: [Ast] -> Env -> Result
lessThan [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Bool (show (x < y))
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err $ "Error in 'a': " ++ errA
        (_, Err errB) -> Err $ "Error in 'b': " ++ errB
        _ -> Err "Error: Comparison requires two integer values."
lessThan _ _ = Err "Error in <: Insufficient arguments"

greaterThan :: [Ast] -> Env -> Result
greaterThan [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Bool (show (x > y))
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
        (Err errA, _) -> Err $ "Error in 'a': " ++ errA
        (_, Err errB) -> Err $ "Error in 'b': " ++ errB
        _ -> Err "Error: Comparison requires two integer values."
greaterThan _ _ = Err "Error in >: Insufficient arguments"
