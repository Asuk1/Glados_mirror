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

testEqualWithValidSymbols :: Spec
testEqualWithValidSymbols = do
    describe "Equal with valid symbols" $ do
        it "should equal two integers" $ do
            let env = [("x", AstInteger 5), ("y", AstInteger 5)]
            equal [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool True)

testNotEqualWithValidSymbols :: Spec
testNotEqualWithValidSymbols = do
    describe "Not equal with valid symbols" $ do
        it "should not equal two integers" $ do
            let env = [("x", AstInteger 5), ("y", AstInteger 10)]
            equal [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool False)

testLessThanWithValidSymbols :: Spec
testLessThanWithValidSymbols = do
    describe "Less than with valid symbols" $ do
        it "should less than two integers" $ do
            let env = [("x", AstInteger 5), ("y", AstInteger 10)]
            lessThan [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool True)

testGreaterThanWithValidSymbols :: Spec
testGreaterThanWithValidSymbols = do
    describe "Greater than with valid symbols" $ do
        it "should greater than two integers" $ do
            let env = [("x", AstInteger 10), ("y", AstInteger 5)]
            greaterThan [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool True)

spec :: Spec
spec = do
    testAddWithValidSymbols


