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

instance Eq Ast where
    (AstInteger x) == (AstInteger y) = x == y
    (AstSymbol s1) == (AstSymbol s2) = s1 == s2
    (AstBoolean b1) == (AstBoolean b2) = b1 == b2
    (AstCall a1) == (AstCall a2) = a1 == a2
    (AstDefine e1 a1) == (AstDefine e2 a2) = e1 == e2 && a1 == a2
    (AstLambda p1 a1) == (AstLambda p2 a2) = p1 == p2 && a1 == a2
    _ == _ = False


type Env = [(String, Ast)]
