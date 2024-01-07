module Lib
    ( someFunc,
        Ast (..),
        Env,
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"



data Ast = AstInteger Int
    | AstSymbol String
    | AstBoolean Bool
    | AstCall [Ast]
    | AstDefine (Either String [String]) Ast
    | AstLambda [String] Ast deriving (Show)



type Env = [(String, Ast)]
