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
        mult,
        inferior,
        division,
        modulo,
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


-- if   |✘|
-- +    |✔|
-- -    |✔|
-- *    |✔|
-- div  |✔|
-- mod  |✔|
-- <    |✔|
-- eq?  |✘|




add :: [Ast] -> Env -> Result
add [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x + y)
        (Err errA, _) -> Err ("Error in add 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in add 'b': " ++ errB)
        _ -> Err "Error: Addition requires two integer values."
add _ _ = Err "Error in add: Insufficient arguments."


sub :: [Ast] -> Env -> Result
sub [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x - y)
        (Err errA, _) -> Err ("Error in sub 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in sub 'b': " ++ errB)
        _ -> Err "Error: Subtract requires two integer values."
sub _ _ = Err "Error in sub: Insufficient arguments."


mult :: [Ast] -> Env -> Result
mult [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x * y)
        (Err errA, _) -> Err ("Error in mult 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in mult 'b': " ++ errB)
        _ -> Err "Error: Multiplication requires two integer values."
mult _ _ = Err "Error in mult: Insufficient arguments."


inferior :: [Ast] -> Env -> Result
inferior [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Bool (if x < y then "#t" else "#f")
        (Err errA, _) -> Err ("Error in inferior 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in inferior 'b': " ++ errB)
        _ -> Err "Error: Comparison requires two integer values."
inferior _ _ = Err "Error in inferior: Insufficient arguments."


division :: [Ast] -> Env -> Result
division [a, b] env =
    case (getValue a env, getValue b env) of
        (_, Value 0) -> Err ("Error: Division by 0 is prohibited")
        (Value x, Value y) -> Value (x`div`y)
        (Err errA, _) -> Err ("Error in division 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in division 'b': " ++ errB)
        _ -> Err "Error: Division requires two integer values."
division _ _ = Err "Error in division: Insufficient arguments."


modulo :: [Ast] -> Env -> Result
modulo [a, b] env =
    case (getValue a env, getValue b env) of
        (_, Value 0) -> Err ("Error: Modulo by 0 is prohibited")
        (Value x, Value y) -> Value (x`mod`y)
        (Err errA, _) -> Err ("Error in modulo 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in modulo 'b': " ++ errB)
        _ -> Err "Error: Modulo requires two integer values."
modulo _ _ = Err "Error in modulo: Insufficient arguments."
