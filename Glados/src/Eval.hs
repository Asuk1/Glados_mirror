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
        funcCall,
        callFunc,

        addKeyVal,
        setFunctionInEnv,
        callFunction,
        isBuild,
        callUserFunction,
        callBuildFunction,
        functionValue,



        eval,
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




addKeyVal :: String -> Ast -> Env -> Env
addKeyVal key val env = (key, val) : env

setFunctionInEnv :: [String] -> [Ast] -> Env -> Either Env String
setFunctionInEnv (_:_) [] _ = Right "Too few arguments"
setFunctionInEnv [] (_:_) _ = Right "Too many arguments"
setFunctionInEnv [] [] env = Left env
setFunctionInEnv (x:xs) (y:ys) env =
    case eval y env of
        IntRes s -> setFunctionInEnv xs ys (addKeyVal x (AstInteger s) env)
        BoolRes s -> setFunctionInEnv xs ys (addKeyVal x (AstBoolean s) env)
        ErrRes s -> Right s
        _ -> Right ("Func " ++ x ++ ": no expression in body.")


callFunction :: String -> [String] -> Ast -> [Ast] -> Env -> Function
callFunction name params body actualArgs env =
    case length params == length actualArgs of
        False -> ErrFunc ("Calling " ++ name ++ " with an incorrect number of arguments.")
        True ->
            case setFunctionInEnv params actualArgs env of
                Left newEnv ->
                    case eval body newEnv of
                        IntRes v -> IntFunc v
                        BoolRes v -> BoolFunc v
                        ErrRes err -> ErrFunc err
                        _ -> ErrFunc (name ++ ": incorrect return type.")
                Right err -> ErrFunc err

funcCall :: String -> Ast -> [Ast] -> Env -> Function
funcCall name (AstDefine (Right params) body) actualArgs env =
    callFunction name params body actualArgs env
funcCall name (AstLambda lambdaParams body) actualArgs env =
    callFunction name lambdaParams body actualArgs env
funcCall name _ _ _ = ErrFunc ("Invalid call " ++ name ++ ".")



callFunc :: [Ast] -> Env -> Function
callFunc [AstLambda params body] env =
    case length params == 0 of
        True -> case eval body env of
            IntRes v -> IntFunc v
            BoolRes v -> BoolFunc v
            ErrRes err -> ErrFunc err
            _ -> ErrFunc "lambda: incorrect return type."
        False -> ErrFunc "Calling lambda with an incorrect number of arguments."
callFunc (AstSymbol name : args) env =
    case lookup name env of
        Nothing -> ErrFunc (name ++ " is not in the environment.")
        Just val -> funcCall name val args env
callFunc _ _ = ErrFunc "Invalid syntax."



isBuild :: [Ast] -> Env -> Either String Function
isBuild (AstSymbol a:b) env =
    case lookup a [
        ("if", ifFunction),
        ("+", add),
        ("-", sub),
        ("*", mult),
        ("div", division),
        ("mod", modulo),
        ("<", inferior),
        ("eq?", equal)
        ] of
            Nothing -> Left ("Function not found")
            Just ab -> Right (ab b env)
isBuild _ _ = Left "Invalid function call."

callUserFunction :: [Ast] -> Env -> Result
callUserFunction args env =
    case callFunc args env of
        IntFunc result -> IntRes result
        BoolFunc result -> BoolRes result
        ErrFunc err -> ErrRes err

callBuildFunction :: [Ast] -> Env -> Result
callBuildFunction args env =
    case isBuild args env of
        Right (IntFunc result) -> IntRes result
        Right (BoolFunc result) -> BoolRes result
        Right (ErrFunc err) -> ErrRes err
        Left "Function not found" -> callUserFunction args env
        Left err -> ErrRes err

functionValue :: [Ast] -> Env -> Result
functionValue [] _ = ErrRes "Invalid function call"
functionValue args env =
    case callBuildFunction args env of
        IntRes result -> IntRes result
        BoolRes result -> BoolRes result
        ErrRes err -> ErrRes err




eval :: Ast -> Env -> Result
eval (AstInteger x) _ = (IntRes x)
eval (AstBoolean x) _ = (BoolRes x)
eval (AstSymbol x) env = getSymbol x env
eval (AstCall x) env = functionValue x env
eval (AstDefine x xs) env = defineSymbol x xs env
eval (AstLambda _ _) _ = (ExprRes "lambda")



printEval :: [Function] -> IO ()
printEval [] = return ()
printEval (x:xs) = putStrLn (show x) >> printEval xs
