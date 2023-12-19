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
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            add [AstSymbol "x", AstInteger 4] [] `shouldBe` (Err "Error in add 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            add [AstInteger 3, AstSymbol "y"] [] `shouldBe` (Err "Error in add 'b': Symbol 'y' not found in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 3)]
            add [AstSymbol "x", AstBoolean "false"] env `shouldBe` (Err "Error: Addition requires two integer values.")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            add [AstBoolean "true", AstBoolean "false"] [] `shouldBe` (Err "Error: Addition requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            add [AstInteger 3] [] `shouldBe` (Err "Error in add: Insufficient arguments.")
            add [] [] `shouldBe` (Err "Error in add: Insufficient arguments.")


testSubFunction :: Spec
testSubFunction = do
    describe "Subtraction with valid integer" $ do
        it "should subtract two positive integers" $ do
            sub [AstInteger 3, AstInteger 4] [] `shouldBe` (Value (-1))
        it "should subtract two negative integers" $ do
            sub [AstInteger (-3), AstInteger (-4)] [] `shouldBe` (Value 1)
        it "should subtract one positive integer and one negative integer" $ do
            sub [AstInteger 3, AstInteger (-4)] [] `shouldBe` (Value 7)
        it "should subtract one negative integer and one positive integer" $ do
            sub [AstInteger (-3), AstInteger 4] [] `shouldBe` (Value (-7))
    describe "Subtraction with valid symbol" $ do
        it "should subtract two positive integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-1))
        it "should subtract two negative integers" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger (-4))]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 1)
        it "should subtract one positive integer and one negative integer" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger (-4))]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 7)
        it "should subtract one negative integer and one positive integer" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger 4)]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-7))
    describe "Subtraction with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            sub [AstBoolean "true", AstInteger 4] [] `shouldBe` (Err "Error: Subtract requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            sub [AstSymbol "x", AstInteger 4] [] `shouldBe` (Err "Error in sub 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            sub [AstInteger 3, AstSymbol "y"] [] `shouldBe` (Err "Error in sub 'b': Symbol 'y' not found in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 3)]
            sub [AstSymbol "x", AstBoolean "false"] env `shouldBe` (Err "Error: Subtract requires two integer values.")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            sub [AstBoolean "true", AstBoolean "false"] [] `shouldBe` (Err "Error: Subtract requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            sub [AstInteger 3] [] `shouldBe` (Err "Error in sub: Insufficient arguments.")
            sub [] [] `shouldBe` (Err "Error in sub: Insufficient arguments.")


testMultFunction :: Spec
testMultFunction = do
    describe "Multiplication with valid integer" $ do
        it "should multiply two positive integers" $ do
            mult [AstInteger 3, AstInteger 4] [] `shouldBe` (Value 12)
        it "should multiply two negative integers" $ do
            mult [AstInteger (-3), AstInteger (-4)] [] `shouldBe` (Value 12)
        it "should multiply one positive integer and one negative integer" $ do
            mult [AstInteger 3, AstInteger (-4)] [] `shouldBe` (Value (-12))
        it "should multiply one negative integer and one positive integer" $ do
            mult [AstInteger (-3), AstInteger 4] [] `shouldBe` (Value (-12))
    describe "Multiplication with valid symbol" $ do
        it "should multiply two positive integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 12)
        it "should multiply two negative integers" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger (-4))]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 12)
        it "should multiply one positive integer and one negative integer" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger (-4))]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-12))
        it "should multiply one negative integer and one positive integer" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger 4)]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-12))
    describe "Multiplication with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            mult [AstBoolean "true", AstInteger 4] [] `shouldBe` (Err "Error: Multiplication requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            mult [AstSymbol "x", AstInteger 4] [] `shouldBe` (Err "Error in mult 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            mult [AstInteger 3, AstSymbol "y"] [] `shouldBe` (Err "Error in mult 'b': Symbol 'y' not found in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 3)]
            mult [AstSymbol "x", AstBoolean "false"] env `shouldBe` (Err "Error: Multiplication requires two integer values.")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            mult [AstBoolean "true", AstBoolean "false"] [] `shouldBe` (Err "Error: Multiplication requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            mult [AstInteger 3] [] `shouldBe` (Err "Error in mult: Insufficient arguments.")
            mult [] [] `shouldBe` (Err "Error in mult: Insufficient arguments.")

testInferiorFunction :: Spec
testInferiorFunction = do
    describe "Comparison with valid integer values" $ do
        it "should return Bool \"#t\" if 'a' is less than 'b'" $ do
            inferior [AstInteger 3, AstInteger 4] [] `shouldBe` (Bool "#t")
        it "should return Bool \"#f\" if 'a' is greater than 'b'" $ do
            inferior [AstInteger 5, AstInteger 4] [] `shouldBe` (Bool "#f")
        it "should return Bool \"#f\" if 'a' is equal to 'b'" $ do
            inferior [AstInteger 4, AstInteger 4] [] `shouldBe` (Bool "#f")
    describe "Comparison with valid symbol values" $ do
        it "should return Bool \"#t\" if symbol 'x' is less than symbol 'y'" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            inferior [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool "#t")
        it "should return Bool \"#f\" if symbol 'x' is greater than symbol 'y'" $ do
            let env = [("x", AstInteger 5), ("y", AstInteger 4)]
            inferior [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool "#f")
        it "should return Bool \"#f\" if symbol 'x' is equal to symbol 'y'" $ do
            let env = [("x", AstInteger 4), ("y", AstInteger 4)]
            inferior [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool "#f")
    describe "Comparison with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            inferior [AstBoolean "true", AstInteger 4] [] `shouldBe` (Err "Error: Inferior requires two integer values.")
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            inferior [AstSymbol "x", AstBoolean "false"] [] `shouldBe` (Err "Error in inferior 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            inferior [AstBoolean "false", AstSymbol "x"] [] `shouldBe` (Err "Error in inferior 'b': Symbol 'x' not found in the environment.")
    describe "Insufficient arguments." $ do
        it "should return an error if there are insufficient arguments." $ do
            inferior [AstInteger 3] [] `shouldBe` (Err "Error in inferior: Insufficient arguments.")
            inferior [] [] `shouldBe` (Err "Error in inferior: Insufficient arguments.")


testDivFunction :: Spec
testDivFunction = do
    describe "Division with valid integer" $ do
        it "should divide two positive integers" $ do
            division [AstInteger 8, AstInteger 4] [] `shouldBe` (Value 2)
        it "should divide two negative integers" $ do
            division [AstInteger (-8), AstInteger (-4)] [] `shouldBe` (Value 2)
        it "should divide one positive integer and one negative integer" $ do
            division [AstInteger 8, AstInteger (-4)] [] `shouldBe` (Value (-2))
        it "should divide one negative integer and one positive integer" $ do
            division [AstInteger (-8), AstInteger 4] [] `shouldBe` (Value (-2))
    describe "Division with valid symbol" $ do
        it "should divide two positive integers in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger 4)]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 2)
        it "should divide two negative integers in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger (-4))]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 2)
        it "should divide one positive integer and one negative integer in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger (-4))]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-2))
        it "should divide one negative integer and one positive integer in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger 4)]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value (-2))
    describe "Division with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            division [AstBoolean "true", AstInteger 4] [] `shouldBe` (Err "Error: Division requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            division [AstInteger 3, AstBoolean "false"] [] `shouldBe` (Err "Error: Division requires two integer values.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            division [AstSymbol "x", AstInteger 8] [] `shouldBe` (Err "Error in division 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            division [AstInteger 8, AstSymbol "y"] [] `shouldBe` (Err "Error in division 'b': Symbol 'y' not found in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 8)]
            division [AstSymbol "x", AstInteger 0] env `shouldBe` (Err "Error: Division by 0 is prohibited")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            division [AstBoolean "true", AstBoolean "false"] [] `shouldBe` (Err "Error: Division requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            division [AstInteger 8] [] `shouldBe` (Err "Error in division: Insufficient arguments.")
            division [] [] `shouldBe` (Err "Error in division: Insufficient arguments.")

testModFunction :: Spec
testModFunction = do
    describe "Modulo with valid integer" $ do
        it "should calculate the modulo of two positive integers" $ do
            modulo [AstInteger 8, AstInteger 4] [] `shouldBe` (Value 0)
        it "should calculate the modulo of two negative integers" $ do
            modulo [AstInteger (-8), AstInteger (-4)] [] `shouldBe` (Value 0)
        it "should calculate the modulo of one positive integer and one negative integer" $ do
            modulo [AstInteger 8, AstInteger (-4)] [] `shouldBe` (Value 0)
        it "should calculate the modulo of one negative integer and one positive integer" $ do
            modulo [AstInteger (-8), AstInteger 4] [] `shouldBe` (Value 0)

    describe "Modulo with valid symbol" $ do
        it "should calculate the modulo of two positive integers in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger 4)]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 0)
        it "should calculate the modulo of two negative integers in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger (-4))]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 0)
        it "should calculate the modulo of one positive integer and one negative integer in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger (-4))]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 0)
        it "should calculate the modulo of one negative integer and one positive integer in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger 4)]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Value 0)

    describe "Modulo with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            modulo [AstBoolean "true", AstInteger 4] [] `shouldBe` (Err "Error: Modulo requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            modulo [AstInteger 3, AstBoolean "false"] [] `shouldBe` (Err "Error: Modulo requires two integer values.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            modulo [AstSymbol "x", AstInteger 8] [] `shouldBe` (Err "Error in modulo 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            modulo [AstInteger 8, AstSymbol "y"] [] `shouldBe` (Err "Error in modulo 'b': Symbol 'y' not found in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 8)]
            modulo [AstSymbol "x", AstInteger 0] env `shouldBe` (Err "Error: Modulo by 0 is prohibited")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            modulo [AstBoolean "true", AstBoolean "false"] [] `shouldBe` (Err "Error: Modulo requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            modulo [AstInteger 8] [] `shouldBe` (Err "Error in modulo: Insufficient arguments.")
            modulo [] [] `shouldBe` (Err "Error in modulo: Insufficient arguments.")

testEqualFunction :: Spec
testEqualFunction = do
    describe "Equality with valid integer values" $ do
        it "should return Bool \"#t\" if 'a' is equal to 'b'" $ do
            equal [AstInteger 4, AstInteger 4] [] `shouldBe` (Bool "#t")
        it "should return Bool \"#f\" if 'a' is not equal to 'b'" $ do
            equal [AstInteger 3, AstInteger 4] [] `shouldBe` (Bool "#f")
    describe "Equality with valid symbol values" $ do
        it "should return Bool \"#t\" if symbol 'x' is equal to symbol 'y'" $ do
            let env = [("x", AstInteger 4), ("y", AstInteger 4)]
            equal [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool "#t")
        it "should return Bool \"#f\" if symbol 'x' is not equal to symbol 'y'" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            equal [AstSymbol "x", AstSymbol "y"] env `shouldBe` (Bool "#f")
    describe "Equality with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            equal [AstBoolean "true", AstInteger 4] [] `shouldBe` (Err "Error: Equal requires two integer values.")
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            equal [AstSymbol "x", AstBoolean "false"] [] `shouldBe` (Err "Error in equal 'a': Symbol 'x' not found in the environment.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            equal [AstBoolean "false", AstSymbol "x"] [] `shouldBe` (Err "Error in equal 'b': Symbol 'x' not found in the environment.")
    describe "Insufficient arguments." $ do
        it "should return an error if there are insufficient arguments." $ do
            equal [AstInteger 3] [] `shouldBe` (Err "Error in equal: Insufficient arguments.")
            equal [] [] `shouldBe` (Err "Error in equal: Insufficient arguments.")


spec :: Spec
spec = do
    testAddFunction
    testSubFunction
    testMultFunction
    testInferiorFunction
    testDivFunction
    testModFunction
    testEqualFunction

