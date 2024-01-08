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
        equal,
        ifFunction,
        callFunc,
        Result (..),
    ) where

import Lib

data Result = Value Int | Boolean Bool | Err String

instance Eq Result where
    (Value x) == (Value y) = x == y
    (Boolean s1) == (Boolean s2) = s1 == s2
    (Err e1) == (Err e2) = e1 == e2
    _ == _ = False

instance Show Result where
    show (Value x) = "Value " ++ show x
    show (Boolean s) = "Bool " ++ show s
    show (Err s) = "Err " ++ show s

getValue :: Ast -> Env -> Result
getValue (AstInteger a) _ = Value a
getValue (AstBoolean a) _ = Boolean a
getValue (AstSymbol s) env =
    case lookup s env of
        Just value -> getValue value env
        Nothing -> Err ("Symbol '" ++ s ++ "' not found in the environment.")
getValue _ _ = Err "Error: Unsupported expression type"



add :: [Ast] -> Env -> Result
add [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Value (x + y)
        (Err errA, Value _) -> Err ("Error in add 'a': " ++ errA)
        (Value _, Err errB) -> Err ("Error in add 'b': " ++ errB)
        (Err errA, Err errB) -> Err $ "Error in 'a': " ++ errA ++ ", Error in 'b': " ++ errB
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
        (Value x, Value y) -> Boolean (x < y)
        (Err errA, _) -> Err ("Error in inferior 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in inferior 'b': " ++ errB)
        _ -> Err "Error: Inferior requires two integer values."
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


equal :: [Ast] -> Env -> Result
equal [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Boolean (x == y)
        (Err errA, _) -> Err ("Error in equal 'a': " ++ errA)
        (_, Err errB) -> Err ("Error in equal 'b': " ++ errB)
        _ -> Err "Error: Equal requires two integer values."
equal _ _ = Err "Error in equal: Insufficient arguments."


ifFunction :: [Ast] -> Env -> Result
ifFunction [a, b, c] env =
    case (getValue a env, getValue b env, getValue c env) of
        (Boolean True, Value x, _) -> Value x
        (Boolean True, Boolean x, _) -> Boolean x
        (Boolean True, Err x, _) -> Err x
        (Boolean False, _, Value y) -> Value y
        (Boolean False, _, Boolean y) -> Boolean y
        (Boolean False, _, Err y) -> Err y
        _ -> Err "Error in if: First argument must be a boolean condition."
ifFunction _ _ = Err "Error in if: Insufficient arguments."

addKeyVal :: String -> Ast -> Env -> Env
addKeyVal key val env = (key, val):env

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

setFuncEnv :: [String] -> [Ast] -> Env -> Either Env String
setFuncEnv (_:_) [] _ = Right "Too few arguments"
setFuncEnv [] (_:_) _ = Right "Too many arguments"
setFuncEnv [] [] env = Left env
setFuncEnv (a:as) (b:bs) env = case getValue b env of
    (Value x) -> setFuncEnv as bs (addKeyVal a (AstInteger x) env)
    (Boolean x) -> setFuncEnv as bs (addKeyVal a (AstInteger (boolToInt x)) env)
    (Err x) -> Right x

getBuiltins :: [(String, [Ast] -> Env -> Result)]
getBuiltins = [
    ("+", add),
    ("-", sub),
    ("*", mult),
    ("<", inferior),
    ("/", division),
    ("%", modulo),
    ("=", equal),
    ("if", ifFunction)
    ]

isBuild :: [Ast] -> Env -> Result
isBuild [] _ = (Boolean False)
isBuild (AstSymbol a:b) env = case lookup a getBuiltins of
                    Nothing -> Boolean False
                    Just ab -> ab b env
isBuild _ _ = Err "Bad call"

funcValue :: [Ast] -> Env -> Result
funcValue [] _ = (Err "Invalid function call")
funcValue x env = case isBuild x env of
    (Boolean False) -> case callFunc x env of
        (Value x) -> (Value x)
        (Boolean x) -> (Boolean x)
        (Err x) -> (Err x)
    (Boolean x) -> (Boolean x)
    (Value x) -> (Value x)
    (Err x) -> (Err x)

callFunc :: [Ast] -> Env -> Result
callFunc (AstLambda a x:[]) env = case (length a) == 0 of
    False -> (Err ("Incorrect Lambda call"))
    True -> eval x env
callFunc (AstLambda a x:b) env = case (length a) == (length b) of
    False -> (Err ("Incorrect Lambda call"))
    True -> case setFuncEnv a b env of
        Left envi -> eval x envi
        Right err -> (Err err)
callFunc _ _ = (Err "Invalid function call")

eval :: Ast -> Env -> Result
eval (AstInteger x) _ = (Value x)
eval (AstBoolean x) _ = (Boolean x)
eval (AstSymbol x) env = getValue (AstSymbol x) env
eval (AstCall x) env = funcValue x env

printEval :: [Result] -> IO ()
printEval [] = return ()
printEval (x:xs) = putStrLn (show x) >> printEval xs
