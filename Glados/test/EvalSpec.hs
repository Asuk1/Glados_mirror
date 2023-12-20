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




spec :: Spec
spec = do
    testAddWithValidSymbols


