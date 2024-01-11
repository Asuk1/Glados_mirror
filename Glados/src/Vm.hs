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
    ) where

import Data.Maybe

data Value = VInt Int | VBool Bool | VOp Op | VFunc [Instruction] deriving (Show, Eq)
data Op = Add | Sub | Mul | Div | Less| Fact | Equal deriving (Show, Eq)
data Instruction = Push Value | Pop | Call | Ret | JumpIfFalse Int | PushArg Int | PushEnv String | Define deriving (Show, Eq)
type Stack = [Value]
type Env = [(String, Value)]
data Ast = AstInteger Int
    | AstSymbol String
    | AstBoolean String
    | AstCall [Ast]
    | AstDefine (Either String [String]) Ast
    | AstLambda [String] Ast deriving (Show, Eq)

opToFunction :: Op -> (Int -> Int -> Int)
opToFunction Add = (+)
opToFunction Sub = (-)
opToFunction Mul = (*)
opToFunction Div = div
opToFunction Less = \a b -> if a < b then 1 else 0
opToFunction Equal = \a b -> if a == b then 1 else 0
opToFunction Fact = \a _ -> if a <= 1 then 1 else a * opToFunction Fact (a - 1) 1

executeInstruction :: Instruction -> Stack -> [Value] -> Env -> Either String (Stack, Int)
executeInstruction Define stack _ _ = Right (stack, 1)
executeInstruction (Push value) stack _ _ = Right (value : stack, 1)
executeInstruction Pop [] _ _ = Left "Error: Not enough arguments on stack"
executeInstruction Pop (_:stack) _ _ = Right (stack, 1)
executeInstruction Call (VOp op : stack) _ _ =
    case performOperation (opToFunction op) stack of
        Right newStack -> Right (newStack, 1)
        Left errorMsg -> Left errorMsg
executeInstruction Call (VFunc func : stack) args env =
    case exec func [] args env 0 of
        Right retStack -> Right (head retStack : stack, 1)
        Left errorMsg -> Left errorMsg
executeInstruction Ret stack _ _ = Right (stack, 1)
executeInstruction (JumpIfFalse offset) (VBool False : stack) _ _ = Right (stack, offset)
executeInstruction (JumpIfFalse _) stack _ _ = Right (stack, 1)
executeInstruction (PushArg index) stack args _ =
    case drop index args of
        (arg:_) -> Right (arg : stack, 1)
        [] -> Left "Error: Argument index out of bounds"
executeInstruction (PushEnv name) stack _ env =
    case lookup name env of
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
        case executeInstruction (instructions !! ip) stack args env of
            Right (newStack, offset) -> exec instructions newStack args env (ip + offset)
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
    _ -> Nothing

parseOp :: String -> Op
parseOp op = case op of
    "Add" -> Add
    "Sub" -> Sub
    "Mul" -> Mul
    "Div" -> Div
    "Less" -> Less
    "Fact" -> Fact
    "Equal" -> Equal
    _ -> error "Unknown operation"

astToInstructions :: Ast -> [Instruction]
astToInstructions (AstInteger n) = [Push (VInt n)]
astToInstructions (AstSymbol s) = [PushEnv s]
astToInstructions (AstBoolean b) = [Push (VBool (read b))]
astToInstructions (AstCall [AstDefine (Left var) body, args]) =
    [PushEnv var] ++ astToInstructions body ++ [Define] ++ astToInstructions args ++ [Call]
astToInstructions (AstCall [AstSymbol name, arg1, arg2]) =
    [PushEnv name] ++ astToInstructions arg1 ++ astToInstructions arg2 ++ [Call]
astToInstructions (AstCall [AstLambda argList body, args]) =
    astToInstructions (AstCall [AstDefine (Right argList) body, args])
astToInstructions (AstCall astList) = concatMap astToInstructions astList
astToInstructions (AstDefine (Right argList) body) =
    astToInstructions (AstCall [AstDefine (Right argList) body])
astToInstructions (AstLambda argList body) =
    [Push (VFunc (astToInstructions (AstCall [AstDefine (Right argList) body])))]

-- main' :: String -> IO ()
-- main' filePath = do
--     contents <- readFile filePath
--     putStrLn "Contenu du fichier :"
--     putStrLn contents
--     let linesOfFile = lines contents
--     let instructions = mapMaybe parseInstruction linesOfFile
--     putStrLn "Instructions lues du fichier :"
--     mapM_ print instructions
--     let initialStack = [VInt (-42)]
--     let env = [("abs", VInt 42)]
--     case exec instructions initialStack [] env 0 of
--         Right resultStack -> case resultStack of
--             (VInt result : _) -> print result
--             _ -> putStrLn "Error: Invalid result on stack"
--         Left errorMsg -> putStrLn errorMsg

main :: IO ()
main = do
    let exampleAst = AstCall [AstDefine (Left "add") (AstCall [AstSymbol "lambda",AstCall [AstSymbol "a",AstSymbol "b"],AstCall [AstSymbol "+",AstSymbol "a",AstSymbol "b"]]),AstCall [AstSymbol "add",AstInteger 3,AstInteger 4]]

    let instructions = astToInstructions exampleAst

    putStrLn "Instructions générées à partir de l'AST :"
    mapM_ print instructions