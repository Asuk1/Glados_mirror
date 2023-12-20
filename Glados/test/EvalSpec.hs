module EvalSpec (spec) where

import Test.Hspec
import Eval
import Lib



testAddWithValidSymbols :: Spec
testAddWithValidSymbols = do
    describe "Addition with valid symbols" $ do
        it "should add two integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 7)



spec :: Spec
spec = do
    testAddWithValidSymbols


