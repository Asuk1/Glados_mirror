module EvalSpec (spec) where

import Test.Hspec
import Eval
import Lib



testAddFunction :: Spec
testAddFunction = do
    describe "Addition with valid integer" $ do
        it "should add two positive integers" $ do
            add [AstInteger 3, AstInteger 4] [] `shouldBe` (Value 7)
        it "should add two negative integers" $ do
            add [AstInteger (-3), AstInteger (-4)] [] `shouldBe` (Value (-7))
        it "should add one positive integer and one negative integer" $ do
            add [AstInteger 3, AstInteger (-4)] [] `shouldBe` (Value (-1))
        it "should add one negative integer and one positive integer" $ do
            add [AstInteger (-3), AstInteger 4] [] `shouldBe` (Value 1)
    describe "Addition with valid symbol" $ do
        it "should add two positive integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 7)
        it "should add two negative integers" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger (-4))]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-7))
        it "should add one positive integer and one negative integer" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger (-4))]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-1))
        it "should add one negative integer and one positive integer" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger 4)]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 1)
    describe "Addition with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            add [AstBoolean "true", AstInteger 4] [] `shouldBe` (Err "Error: Addition requires two integer values.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            add [AstInteger 3, AstSymbol "y"] [] `shouldBe` (Err "Error in 'b': Symbol 'y' not found in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            add [AstSymbol "x", AstInteger 4] [] `shouldBe` (Err "Error in 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 3)]
            add [AstSymbol "x", AstBoolean "false"] env `shouldBe` (Err "Error: Addition requires two integer values.")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            add [AstBoolean "true", AstBoolean "false"] [] `shouldBe` (Err "Error: Addition requires two integer values.")
        it "should return an error if there are insufficient arguments" $ do
            add [AstInteger 3] [] `shouldBe` (Err "Error in add: Insufficient arguments")
            add [] [] `shouldBe` (Err "Error in add: Insufficient arguments")






spec :: Spec
spec = do
    testAddFunction


