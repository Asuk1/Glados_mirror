--
-- EPITECH PROJECT, 2024
-- Glados_mirror
-- File description:
-- Vm
--

data Value = VInt Int | VBool Bool | VOp Op | VFunc [Instruction] deriving (Show, Eq)
data Op = Add | Sub | Mul | Div | Less deriving (Show, Eq)
data Instruction = Push Value | Pop | Call | Ret | JumpIfFalse Int | PushArg Int | PushEnv String deriving (Show, Eq)
type Stack = [Value]
type Env = [(String, Value)]

absCode :: [Instruction]
absCode =
    [ PushArg 0
    , Push (VInt 0)
    , Push (VOp Less)
    , Call
    , JumpIfFalse 2
    , PushArg 0
    , Ret
    , PushArg 0
    , Push (VInt (-1))
    , Push (VOp Mul)
    , Call
    , Ret
    ]

opToFunction :: Op -> (Int -> Int -> Int)
opToFunction Add = (+)
opToFunction Sub = (-)
opToFunction Mul = (*)
opToFunction Div = div
opToFunction Less = \a b -> if a < b then 1 else 0

executeInstruction :: Instruction -> Stack -> [Value] -> Env -> Either String (Stack, Int)
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
performOperation op (VInt a : VInt b : stack) = Right (VInt (op b a) : stack)
performOperation _ [] = Left "Error: Not enough arguments on stack"
performOperation _ _ = Left "Error: Invalid arguments on stack"

exec :: [Instruction] -> Stack -> [Value] -> Env -> Int -> Either String Stack
exec instructions stack args env ip
    | ip >= length instructions = Right stack
    | otherwise =
        case executeInstruction (instructions !! ip) stack args env of
            Right (newStack, offset) -> exec instructions newStack args env (ip + offset)
            Left errorMsg -> Left errorMsg

main :: IO ()
main = do
    let env = [("fact", VFunc absCode)]
        absCode = [PushEnv "fact", PushArg 0, Push (VInt 1), Push (VOp Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (VInt (-1)), Push (VOp Mul), Call, Ret]
        args = [VInt 5]
    case exec absCode [] args env 0 of
        Right resultStack -> print resultStack
        Left errorMsg -> putStrLn errorMsg
