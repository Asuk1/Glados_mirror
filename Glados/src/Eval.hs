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
        division,
        modulo,
        inferior,
        equal,
        ifFunction,
        getSymbol,
        addOrReplaceKey,
        handleFunctionBody,
        handleDefinedSymbolName,
        defineSymbol,
        -- callFunc,
        Function (..),
        Result (..),
    ) where

import Lib

data Function = IntFunc Int | BoolFunc Bool | ErrFunc String
data Result = IntRes Int | EnvRes { getEnv :: Env }| BoolRes Bool | ExprRes String | ErrRes String


instance Eq Function where
    (IntFunc x) == (IntFunc y) = x == y
    (BoolFunc s1) == (BoolFunc s2) = s1 == s2
    (ErrFunc e1) == (ErrFunc e2) = e1 == e2
    _ == _ = False

instance Show Function where
    show (IntFunc x) = "IntFunc " ++ show x
    show (BoolFunc s) = "Bool " ++ show s
    show (ErrFunc s) = "ErrFunc " ++ show s


instance Eq Result where
    (EnvRes n1) == (EnvRes n2) = n1 == n2
    (IntRes n1) == (IntRes n2) = n1 == n2
    (BoolRes n1) == (BoolRes n2) = n1 == n2
    (ExprRes n1) == (ExprRes n2) = n1 == n2
    (ErrRes n1) == (ErrRes n2) = n1 == n2
    _ == _ = False

instance Show Result where
    show (IntRes n) = show n
    show (EnvRes n) = show n
    show (BoolRes n) = show n
    show (ExprRes n) = n
    show (ErrRes n) = n






-- getValue :: Ast -> Env -> Function
-- getValue (AstInteger a) _ = IntFunc a
-- getValue (AstBoolean a) _ = BoolFunc a
-- getValue (AstSymbol s) env =
--     case lookup s env of
--         Just value -> getValue value env
--         Nothing -> ErrFunc ("Symbol '" ++ s ++ "' not found in the environment.")
-- getValue _ _ = ErrFunc "Error: Unsupported expression type"


-- addKeyVal :: String -> Ast -> Env -> Env
-- addKeyVal key val env = (key, val):env

-- boolToInt :: Bool -> Int
-- boolToInt True = 1
-- boolToInt False = 0

-- setFuncEnv :: [String] -> [Ast] -> Env -> Either Env String
-- setFuncEnv (_:_) [] _ = Right "Too few arguments"
-- setFuncEnv [] (_:_) _ = Right "Too many arguments"
-- setFuncEnv [] [] env = Left env
-- setFuncEnv (x:xs) (y:ys) env =
--     case getValue y env of
--         IntFunc s -> setFuncEnv xs ys (addKeyVal x (AstInteger s) env)
--         BoolFunc s -> setFuncEnv xs ys (addKeyVal x (AstInteger (boolToInt s)) env)
--         ErrFunc s -> Right s

-- getBuiltins :: [(String, [Ast] -> Env -> Function)]
-- getBuiltins = [
--     ("+", add),
--     ("-", sub),
--     ("*", mult),
--     ("<", inferior),
--     ("/", division),
--     ("%", modulo),
--     ("=", equal),
--     ("if", ifFunction)
--     ]

-- isBuild :: [Ast] -> Env -> Function
-- isBuild [] _ = (BoolFunc False)
-- isBuild (AstSymbol a:b) env = case lookup a getBuiltins of
--                     Nothing -> BoolFunc False
--                     Just ab -> ab b env
-- isBuild _ _ = ErrFunc "Bad call"

-- funcValue :: [Ast] -> Env -> Function
-- funcValue [] _ = (ErrFunc "Invalid function call")
-- funcValue x env = case isBuild x env of
--     (BoolFunc False) -> case callFunc x env of
--         (ErrFunc err) -> (ErrFunc err)
--         (IntFunc a) -> (IntFunc a)
--         (BoolFunc a) -> (BoolFunc a)
--     (BoolFunc a) -> (BoolFunc a)
--     (ErrFunc err) -> (ErrFunc err)
--     _ -> (ErrFunc "Invalid function call")


-- callFunc :: [Ast] -> Env -> Function
-- callFunc (AstLambda a x:[]) env = case (length a) == 0 of
--     False -> (ErrFunc ("Incorrect Lambda call"))
--     True -> eval x env
-- callFunc (AstLambda a x:b) env = case (length a) == (length b) of
--     False -> (ErrFunc ("Incorrect Lambda call"))
--     True -> case setFuncEnv a b env of
--         Left envi -> eval x envi
--         Right err -> (ErrFunc err)
-- callFunc _ _ = (ErrFunc "Invalid function call")





getSymbol :: String -> Env -> Result
getSymbol str env =
    case lookup str env of
        Nothing -> ErrRes (str ++ " is not in the environment.")
        Just (AstInteger value) -> IntRes value
        Just (AstBoolean value) -> BoolRes value
        Just (AstSymbol symbol) -> eval (AstSymbol symbol) env
        Just (AstDefine _ _) -> ExprRes ("function define " ++ str ++ ".")
        Just (AstLambda _ _) -> ExprRes ("function lambda " ++ str ++ ".")


add :: [Ast] -> Env -> Function
add [a, b] env =
    case (eval a env, eval b env) of
        (IntRes x, IntRes y) -> IntFunc (x + y)
        (ErrRes errA, _) -> ErrFunc ("Error in add 'a': " ++ errA)
        (_, ErrRes errB) -> ErrFunc ("Error in add 'b': " ++ errB)
        _ -> ErrFunc "Error: Addition requires two integer values."
add _ _ = ErrFunc "Error in add: Insufficient arguments."

sub :: [Ast] -> Env -> Function
sub [a, b] env =
    case (eval a env, eval b env) of
        (IntRes x, IntRes y) -> IntFunc (x - y)
        (ErrRes errA, _) -> ErrFunc ("Error in sub 'a': " ++ errA)
        (_, ErrRes errB) -> ErrFunc ("Error in sub 'b': " ++ errB)
        _ -> ErrFunc "Error: Subtract requires two integer values."
sub _ _ = ErrFunc "Error in sub: Insufficient arguments."

mult :: [Ast] -> Env -> Function
mult [a, b] env =
    case (eval a env, eval b env) of
        (IntRes x, IntRes y) -> IntFunc (x * y)
        (ErrRes errA, _) -> ErrFunc ("Error in mult 'a': " ++ errA)
        (_, ErrRes errB) -> ErrFunc ("Error in mult 'b': " ++ errB)
        _ -> ErrFunc "Error: Multiplication requires two integer values."
mult _ _ = ErrFunc "Error in mult: Insufficient arguments."

division :: [Ast] -> Env -> Function
division [a, b] env =
    case (eval a env, eval b env) of
        (_, IntRes 0) -> ErrFunc ("Error: Division by 0 is prohibited")
        (IntRes x, IntRes y) -> IntFunc (x`div`y)
        (ErrRes errA, _) -> ErrFunc ("Error in division 'a': " ++ errA)
        (_, ErrRes errB) -> ErrFunc ("Error in division 'b': " ++ errB)
        _ -> ErrFunc "Error: Division requires two integer values."
division _ _ = ErrFunc "Error in division: Insufficient arguments."

modulo :: [Ast] -> Env -> Function
modulo [a, b] env =
    case (eval a env, eval b env) of
        (_, IntRes 0) -> ErrFunc ("Error: Modulo by 0 is prohibited")
        (IntRes x, IntRes y) -> IntFunc (x`mod`y)
        (ErrRes errA, _) -> ErrFunc ("Error in modulo 'a': " ++ errA)
        (_, ErrRes errB) -> ErrFunc ("Error in modulo 'b': " ++ errB)
        _ -> ErrFunc "Error: Modulo requires two integer values."
modulo _ _ = ErrFunc "Error in modulo: Insufficient arguments."

inferior :: [Ast] -> Env -> Function
inferior [a, b] env =
    case (eval a env, eval b env) of
        (IntRes x, IntRes y) -> BoolFunc (x < y)
        (ErrRes errA, _) -> ErrFunc ("Error in inferior 'a': " ++ errA)
        (_, ErrRes errB) -> ErrFunc ("Error in inferior 'b': " ++ errB)
        _ -> ErrFunc "Error: Inferior requires two integer values."
inferior _ _ = ErrFunc "Error in inferior: Insufficient arguments."

equal :: [Ast] -> Env -> Function
equal [a, b] env =
    case (eval a env, eval b env) of
        (IntRes x, IntRes y) -> BoolFunc (x == y)
        (ErrRes errA, _) -> ErrFunc ("Error in equal 'a': " ++ errA)
        (_, ErrRes errB) -> ErrFunc ("Error in equal 'b': " ++ errB)
        _ -> ErrFunc "Error: Equal requires two integer values."
equal _ _ = ErrFunc "Error in equal: Insufficient arguments."

ifFunction :: [Ast] -> Env -> Function
ifFunction [a, b, c] env =
    case (eval a env, eval b env, eval c env) of
        (BoolRes True, IntRes x, _) -> IntFunc x
        (BoolRes True, BoolRes x, _) -> BoolFunc x
        (BoolRes True, ErrRes x, _) -> ErrFunc x
        (BoolRes False, _, IntRes y) -> IntFunc y
        (BoolRes False, _, BoolRes y) -> BoolFunc y
        (BoolRes False, _, ErrRes y) -> ErrFunc y
        _ -> ErrFunc "Error in if: First argument must be a boolean condition."
ifFunction _ _ = ErrFunc "Error in if: Insufficient arguments."



addOrReplaceKey :: String -> Ast -> Env -> Env
addOrReplaceKey key val env = (key, val) : filter (\(k, _) -> k /= key) env

handleFunctionBody :: String -> Ast -> Env -> Result
handleFunctionBody a body env =
    case eval body env of
        ErrRes err -> ErrRes err
        IntRes val -> EnvRes (addOrReplaceKey a (AstInteger val) env)
        BoolRes val -> EnvRes (addOrReplaceKey a (AstBoolean val) env)
        ExprRes _ -> EnvRes (addOrReplaceKey a body env)

handleDefinedSymbolName :: String -> [String] -> Ast -> Env -> Result
handleDefinedSymbolName a params body env =
    EnvRes (addOrReplaceKey a (AstDefine (Right params) body) env)

defineSymbol :: Either String [String] -> Ast -> Env -> Result
defineSymbol (Left a) body env =
    case body of
        AstDefine _ _ -> ErrRes ("Cannot assign to symbol '" ++ a ++ "'. Functions cannot be assigned directly.")
        _ -> handleFunctionBody a body env
defineSymbol (Right []) _ _ = ErrRes "Symbol name is not defined."
defineSymbol (Right (a:params)) body env = handleDefinedSymbolName a params body env








eval :: Ast -> Env -> Result
eval (AstInteger x) _ = (IntRes x)
eval (AstBoolean x) _ = (BoolRes x)
eval (AstSymbol x) env = getSymbol x env
eval (AstCall _) _ = ErrRes "Call not supported yet"
eval (AstDefine x xs) env = defineSymbol x xs env
eval (AstLambda _ _) _ = ErrRes "Lambda not supported yet"



printEval :: [Function] -> IO ()
printEval [] = return ()
printEval (x:xs) = putStrLn (show x) >> printEval xs
