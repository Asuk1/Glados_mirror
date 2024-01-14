--
-- EPITECH PROJECT, 2024
-- Glados_mirror
-- File description: Vm
--

module Vm (
    Op(..),
    Value(..),
    Instruction(..),
    Env,
    opToFunction,
    performOperation,
    executeInstruction,
    exec,
    parseInstruction,
    parseOp,
    astToInstructions,
    compiler,
    factOp,
    divOp,
    subOp,
    executer,
    ) where

import Data.Maybe (mapMaybe)

data Value = VInt Int | VBool Bool | VOp Op | VFunc [Instruction] deriving (Show, Eq)
data Op = Add | Sub | Mul | Div | Less | Bigger | Fact | Succ | Equal deriving (Show, Eq)
data Instruction = Push Value | Pop | Call | Ret | JumpIfFalse Int | PushArg Int | PushEnv String | Define deriving (Show, Eq)
type Stack = [Value]
type Env = [(String, Value)]
data Ast = AstInteger Int
    | AstSymbol String
    | AstBoolean String
    | AstCall [Ast]
    | AstDefine (Either String [String]) Ast
    | AstLambda [String] Ast deriving (Read, Show, Eq)

type UnaryOp = Int -> Int
type BinaryOp = Int -> Int -> Int

opToFunction :: Op -> Either UnaryOp BinaryOp
opToFunction Add = Right (+)
opToFunction Sub = Right subOp
opToFunction Mul = Right (*)
opToFunction Div = Right divOp
opToFunction Less = Right (\a b -> if a > b then 1 else 0)
opToFunction Bigger = Right (\a b -> if a < b then 1 else 0)
opToFunction Equal = Right (\a b -> if a == b then 1 else 0)
opToFunction Fact = Left factOp

factOp :: UnaryOp
factOp a =
    if a <= 1 then
        1
    else
        a * factOp (a - 1)

divOp :: BinaryOp
divOp b a
    | b == 0    = error "Division by zero"
    | otherwise = fromIntegral a `div'` fromIntegral b
    where div' :: Double -> Double -> Int
          div' x y = round (x / y)

subOp :: BinaryOp
subOp a b = b - a

executeInstruction :: Instruction -> [Instruction]-> Stack -> [Value] -> Env -> Either String (Stack, Int)
executeInstruction Define _ stack _ _ = Right (stack, 1)
executeInstruction (Push value) _ stack _ _ = Right (value : stack, 1)
executeInstruction Pop _ [] _ _ = Left "Error: Not enough arguments on stack"
executeInstruction Pop _ (_:stack) _ _ = Right (stack, 1)
executeInstruction Call _ (VOp op : stack) _ _ =
    case opToFunction op of
        Left unaryOp ->
            case stack of
                (VInt a : rest) -> Right (VInt (unaryOp a) : rest, 1)
                _ -> Left "Error: Not enough arguments for unary operation"
        Right binaryOp ->
            case stack of
                (VInt a : VInt b : rest) -> Right (VInt (binaryOp a b) : rest, 1)
                _ -> Left "Error: Invalid arguments for binary operation"
executeInstruction Call _ (VFunc func : stack) args env =
    case exec func [] args env 0 of
        Right retStack -> Right (head retStack : stack, 1)
        Left errorMsg -> Left errorMsg
executeInstruction Ret _ stack _ _ = Right (stack, 1)
executeInstruction (JumpIfFalse offset) _ (VBool False : stack) _ _ = Right (stack, offset)
executeInstruction (JumpIfFalse _) _ stack _ _ = Right (stack, 1)
executeInstruction (PushArg index) _ stack args _ =
    case drop index args of
        (arg:_) -> Right (arg : stack, 1)
        [] -> Left "Error: Argument index out of bounds"
executeInstruction (PushEnv name) instructions stack _ env =
    case lookup name env of
        Just (VFunc funcInstructions) ->
            case exec funcInstructions stack [] (createEnv instructions stack) 0 of
                Right retStack -> Right (retStack, 1)
                Left errorMsg -> Left errorMsg
        Just value -> Right (value : stack, 1)
        Nothing -> Left "Error: Variable not found"

performOperation :: (Int -> Int -> Int) -> Stack -> Either String Stack
performOperation op (VInt a : VInt b : stack) = Right (VInt (op a b) : stack)
performOperation _ [] = Left "Error: Not enough arguments on stack"
performOperation _ _ = Left "Error: Invalid arguments on stack"

exec :: [Instruction] -> Stack -> [Value] -> Env -> Int -> Either String Stack
exec instructions stack args env ip
    | ip >= length instructions = Right stack
    | otherwise =
        case executeInstruction (instructions !! ip) instructions stack args env of
            Right (newStack, offset) -> exec instructions newStack args (createEnv instructions newStack) (ip + offset)
            Left errorMsg -> Left errorMsg

parseInstruction :: String -> Maybe Instruction
parseInstruction line = case words line of
    ("Push" : "VInt" : n : []) -> Just $ Push (VInt (read n))
    ("Push" : "VBool" : b : []) -> Just $ Push (VBool (read b))
    ("Push" : "VOp" : op : []) -> Just $ Push (VOp (parseOp op))
    ("Pop" : []) -> Just Pop
    ("Call" : []) -> Just Call
    ("Ret" : []) -> Just Ret
    ("JumpIfFalse" : offset : []) -> Just $ JumpIfFalse (read offset)
    ("PushArg" : index : []) -> Just $ PushArg (read index)
    ("PushEnv" : name : []) -> Just $ PushEnv name
    ("Define" : []) -> Just Define
    _ -> Nothing

parseOp :: String -> Op
parseOp op = case op of
    "Add" -> Add
    "Sub" -> Sub
    "Mul" -> Mul
    "Div" -> Div
    "Less" -> Less
    "Equal" -> Equal
    "Bigger" -> Bigger
    "Fact" -> Fact
    _ -> error "Unknown operation"

createEnv :: [Instruction] -> Stack -> Env
createEnv instructions stack = go instructions stack []
  where
    go :: [Instruction] -> Stack -> Env -> Env
    go [] _ env = env
    go (Define : PushEnv var : Push value : rest) stack env =
        go rest stack ((var, value) : env)
    go (PushEnv name : PushEnv var : x : y : z : j : rest) stack env =
        go rest stack ((name, VFunc [x, y, z, j]) : (var, (head (if null stack then [VInt 0] else stack))) : env)
    go (_ : rest) stack env = go rest stack env

astToInstructions :: Ast -> [Instruction]
astToInstructions (AstInteger n) = [Push (VInt n)]
astToInstructions (AstSymbol "+") = [Push (VOp Add)]
astToInstructions (AstSymbol "-") = [Push (VOp Sub)]
astToInstructions (AstSymbol "*") = [Push (VOp Mul)]
astToInstructions (AstSymbol "/") = [Push (VOp Div)]
astToInstructions (AstSymbol "fact") = [Push (VOp Fact)]
astToInstructions (AstSymbol "<") = [Push (VOp Less)]
astToInstructions (AstSymbol ">") = [Push (VOp Bigger)]
astToInstructions (AstSymbol "=") = [Push (VOp Equal)]
astToInstructions (AstSymbol s) = [PushEnv s]
astToInstructions (AstBoolean b) = [Push (VBool (read b))]
astToInstructions (AstDefine (Left var) body) =
    [Define] ++ PushEnv var : astToInstructions body
astToInstructions (AstCall [AstSymbol "define", AstCall [AstSymbol func, AstSymbol var], rest]) =
    [Define] ++ [PushEnv func] ++ PushEnv var : astToInstructions rest
astToInstructions (AstCall [name, arg1, arg2]) =
    astToInstructions arg1 ++ astToInstructions arg2 ++ astToInstructions name ++ [Call]
astToInstructions (AstCall [AstSymbol "fact", arg1]) =
    astToInstructions arg1 ++ astToInstructions (AstSymbol "fact") ++ [Call]
astToInstructions (AstCall [AstSymbol name, arg1]) =
    astToInstructions arg1 ++ [PushEnv name]
astToInstructions (AstCall astList) = concatMap astToInstructions astList
astToInstructions (AstDefine (Right argList) body) =
    astToInstructions (AstCall [AstDefine (Right argList) body])
astToInstructions (AstLambda _ body) =
    [Push (VFunc (astToInstructions body))]
astToInstructions _ = []

removeUnwantedChars :: String -> String
removeUnwantedChars = filter (\c -> c /= '(' && c /= ')' && c /= '"')

compiler :: String -> IO ()
compiler filePath = do

    contents <- readFile filePath
    let astList = read contents :: [Ast]
    let instructions = concatMap astToInstructions astList

    let outputFilePath = "instructions.txt"
    writeFile outputFilePath (unlines $ map show instructions)

executer :: String -> IO ()
executer filePath = do

    contents <- readFile filePath
    let tmp = removeUnwantedChars contents
    let instruction = mapMaybe parseInstruction (lines tmp)

    let initialStack = []
    let env = createEnv instruction initialStack

    case exec instruction initialStack [VInt (-42)] env 0 of
        Right resultStack -> case resultStack of
            (VInt result : _) -> do
                putStrLn $ show result
            _ ->
                putStrLn $ show resultStack
        Left errorMsg -> putStrLn errorMsg
