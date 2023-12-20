module EvalSpec (spec) where

import Test.Hspec
import Eval
import Lib
import Distribution.Simple.Test (test)



testAddWithValidSymbols :: Spec
testAddWithValidSymbols = do
    describe "Addition with valid symbols" $ do
        it "should add two integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 7)

testSubtractWithValidSymbols :: Spec
testSubtractWithValidSymbols = do
    describe "Subtraction with valid symbols" $ do
        it "should subtract two integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            subtract [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-1))

testDivideWithValidSymbols :: Spec
testDivideWithValidSymbols = do
    describe "Division with valid symbols" $ do
        it "should divide two integers" $ do
            let env = [("x", AstInteger 10), ("y", AstInteger 2)]
            divide [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 5)

testMultiplyWithValidSymbols :: Spec
testMultiplyWithValidSymbols = do
    describe "Multiplication with valid symbols" $ do
        it "should multiply two integers" $ do
            let env = [("x", AstInteger 10), ("y", AstInteger 10)]
            multiply [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 100)

testModuloWithValidSymbols :: Spec
testModuloWithValidSymbols = do
    describe "Modulo with valid symbols" $ do
        it "should modulo two integers" $ do
            let env = [("x", AstInteger 5), ("y", AstInteger 10)]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 5)
equal :: [Ast] -> Env -> Result
equal [a, b] env =
    case (getValue a env, getValue b env) of
        (Value x, Value y) -> Bool (show x == show y)
        (Err errA, Err errB) -> Err ["Error in 'a': " ++ errA, "Error in 'b': " ++ errB]
        (Err errA, _) -> Err ["Error in 'a': " ++ errA]
        (_, Err errB) -> Err ["Error in 'b': " ++ errB]
        _ -> Err ["Error: Equality comparison requires two values."]
equal _ _ = Err ["Error in equal: Insufficient arguments"]

spec :: Spec
spec = do
    testAddWithValidSymbols


