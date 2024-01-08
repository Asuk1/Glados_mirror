--
-- EPITECH PROJECT, 2024
-- Glados_mirror
-- File description:
-- Vm
--

data Value = VInt Int | VBool Bool | VOp Op | VFunc [Instruction] deriving (Show, Eq)
data Op = Add | Sub | Mul | Div | Less deriving (Show, Eq)
data Instruction = Push Value | Pop | Call | Ret | JumpIfFalse Int | PushArg Int deriving (Show, Eq)
type Stack = [Value]

opToFunction :: Op -> (Int -> Int -> Int)
opToFunction Add = (+)
opToFunction Sub = (-)
opToFunction Mul = (*)
opToFunction Div = div
opToFunction Less = \a b -> if a < b then 1 else 0

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

executeInstruction :: Instruction -> Stack -> [Value] -> Either String (Stack, Int)
executeInstruction (Push v) stack _ = Right (v : stack, 1)
executeInstruction Pop [] _ = Left "Error: Pop on empty stack"
executeInstruction Pop (_:stack) _ = Right (stack, 1)
executeInstruction Call (VOp op : stack) _ =
    case performOperation (opToFunction op) stack of
        Right newStack -> Right (newStack, 1)
        Left errorMsg -> Left errorMsg
executeInstruction Call (VFunc func : stack) args =
    case exec func [] args 0 of
        Right retStack -> Right (head retStack : stack, 1)
        Left errorMsg -> Left errorMsg
executeInstruction Ret stack _ = Right (stack, 1)
executeInstruction (JumpIfFalse offset) (VBool False : stack) _ = Right (stack, offset)
executeInstruction (JumpIfFalse _) stack _ = Right (stack, 1)
executeInstruction (PushArg index) stack args =
    case drop index args of
        (arg:_) -> Right (arg : stack, 1)
        [] -> Left "Error: Argument index out of bounds"

performOperation :: (Int -> Int -> Int) -> Stack -> Either String Stack
performOperation op (VInt a : VInt b : stack) = Right (VInt (op b a) : stack)
performOperation _ [] = Left "Error: Not enough arguments on stack"
performOperation _ _ = Left "Error: Invalid arguments on stack"

exec :: [Instruction] -> Stack -> [Value] -> Int -> Either String Stack
exec instructions stack args ip
    | ip >= length instructions = Right stack
    | otherwise =
        case executeInstruction (instructions !! ip) stack args of
            Right (newStack, offset) -> exec instructions newStack args (ip + offset)
            Left errorMsg -> Left errorMsg

main :: IO ()
main = do
    let absCode = [PushArg 0, Push (VInt 0), Push (VOp Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (VInt (-1)), Push (VOp Mul), Call, Ret]
    let args = [VInt (-42)]
    let result = exec [Push (VFunc absCode), Call, Ret] [] args 0
    case result of
        Right stack -> print stack
        Left errorMsg -> putStrLn errorMsg
