module EvalSpec (spec) where

import Test.Hspec
import Eval
import Lib

testAddFunction :: Spec
testAddFunction = do
    describe "Addition with valid integer" $ do
        it "should add two positive integers" $ do
            add [AstInteger 3, AstInteger 4] [] `shouldBe` (IntFunc 7)
        it "should add two negative integers" $ do
            add [AstInteger (-3), AstInteger (-4)] [] `shouldBe` (IntFunc (-7))
        it "should add one positive integer and one negative integer" $ do
            add [AstInteger 3, AstInteger (-4)] [] `shouldBe` (IntFunc (-1))
        it "should add one negative integer and one positive integer" $ do
            add [AstInteger (-3), AstInteger 4] [] `shouldBe` (IntFunc 1)
    describe "Addition with valid symbol" $ do
        it "should add two positive integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 7)
        it "should add two negative integers" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger (-4))]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-7))
        it "should add one positive integer and one negative integer" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger (-4))]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-1))
        it "should add one negative integer and one positive integer" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger 4)]
            add [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 1)
    describe "Addition with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            add [AstBoolean True, AstInteger 4] [] `shouldBe` (ErrFunc "Error: Addition requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            add [AstSymbol "x", AstInteger 4] [] `shouldBe` (ErrFunc "Error in add 'a': x is not in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            add [AstInteger 3, AstSymbol "y"] [] `shouldBe` (ErrFunc "Error in add 'b': y is not in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 3)]
            add [AstSymbol "x", AstBoolean False] env `shouldBe` (ErrFunc "Error: Addition requires two integer values.")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            add [AstBoolean True, AstBoolean False] [] `shouldBe` (ErrFunc "Error: Addition requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            add [AstInteger 3] [] `shouldBe` (ErrFunc "Error in add: Insufficient arguments.")
            add [] [] `shouldBe` (ErrFunc "Error in add: Insufficient arguments.")

testSubFunction :: Spec
testSubFunction = do
    describe "Subtraction with valid integer" $ do
        it "should subtract two positive integers" $ do
            sub [AstInteger 3, AstInteger 4] [] `shouldBe` (IntFunc (-1))
        it "should subtract two negative integers" $ do
            sub [AstInteger (-3), AstInteger (-4)] [] `shouldBe` (IntFunc 1)
        it "should subtract one positive integer and one negative integer" $ do
            sub [AstInteger 3, AstInteger (-4)] [] `shouldBe` (IntFunc 7)
        it "should subtract one negative integer and one positive integer" $ do
            sub [AstInteger (-3), AstInteger 4] [] `shouldBe` (IntFunc (-7))
    describe "Subtraction with valid symbol" $ do
        it "should subtract two positive integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-1))
        it "should subtract two negative integers" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger (-4))]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 1)
        it "should subtract one positive integer and one negative integer" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger (-4))]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 7)
        it "should subtract one negative integer and one positive integer" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger 4)]
            sub [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-7))
    describe "Subtraction with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            sub [AstBoolean True, AstInteger 4] [] `shouldBe` (ErrFunc "Error: Subtract requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            sub [AstSymbol "x", AstInteger 4] [] `shouldBe` (ErrFunc "Error in sub 'a': x is not in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            sub [AstInteger 3, AstSymbol "y"] [] `shouldBe` (ErrFunc "Error in sub 'b': y is not in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 3)]
            sub [AstSymbol "x", AstBoolean False] env `shouldBe` (ErrFunc "Error: Subtract requires two integer values.")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            sub [AstBoolean True, AstBoolean False] [] `shouldBe` (ErrFunc "Error: Subtract requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            sub [AstInteger 3] [] `shouldBe` (ErrFunc "Error in sub: Insufficient arguments.")
            sub [] [] `shouldBe` (ErrFunc "Error in sub: Insufficient arguments.")

testMultFunction :: Spec
testMultFunction = do
    describe "Multiplication with valid integer" $ do
        it "should multiply two positive integers" $ do
            mult [AstInteger 3, AstInteger 4] [] `shouldBe` (IntFunc 12)
        it "should multiply two negative integers" $ do
            mult [AstInteger (-3), AstInteger (-4)] [] `shouldBe` (IntFunc 12)
        it "should multiply one positive integer and one negative integer" $ do
            mult [AstInteger 3, AstInteger (-4)] [] `shouldBe` (IntFunc (-12))
        it "should multiply one negative integer and one positive integer" $ do
            mult [AstInteger (-3), AstInteger 4] [] `shouldBe` (IntFunc (-12))
    describe "Multiplication with valid symbol" $ do
        it "should multiply two positive integers" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 12)
        it "should multiply two negative integers" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger (-4))]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 12)
        it "should multiply one positive integer and one negative integer" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger (-4))]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-12))
        it "should multiply one negative integer and one positive integer" $ do
            let env = [("x", AstInteger (-3)), ("y", AstInteger 4)]
            mult [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-12))
    describe "Multiplication with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            mult [AstBoolean True, AstInteger 4] [] `shouldBe` (ErrFunc "Error: Multiplication requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            mult [AstSymbol "x", AstInteger 4] [] `shouldBe` (ErrFunc "Error in mult 'a': x is not in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            mult [AstInteger 3, AstSymbol "y"] [] `shouldBe` (ErrFunc "Error in mult 'b': y is not in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 3)]
            mult [AstSymbol "x", AstBoolean False] env `shouldBe` (ErrFunc "Error: Multiplication requires two integer values.")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            mult [AstBoolean True, AstBoolean False] [] `shouldBe` (ErrFunc "Error: Multiplication requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            mult [AstInteger 3] [] `shouldBe` (ErrFunc "Error in mult: Insufficient arguments.")
            mult [] [] `shouldBe` (ErrFunc "Error in mult: Insufficient arguments.")

testDivFunction :: Spec
testDivFunction = do
    describe "Division with valid integer" $ do
        it "should divide two positive integers" $ do
            division [AstInteger 8, AstInteger 4] [] `shouldBe` (IntFunc 2)
        it "should divide two negative integers" $ do
            division [AstInteger (-8), AstInteger (-4)] [] `shouldBe` (IntFunc 2)
        it "should divide one positive integer and one negative integer" $ do
            division [AstInteger 8, AstInteger (-4)] [] `shouldBe` (IntFunc (-2))
        it "should divide one negative integer and one positive integer" $ do
            division [AstInteger (-8), AstInteger 4] [] `shouldBe` (IntFunc (-2))
    describe "Division with valid symbol" $ do
        it "should divide two positive integers in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger 4)]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 2)
        it "should divide two negative integers in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger (-4))]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 2)
        it "should divide one positive integer and one negative integer in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger (-4))]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-2))
        it "should divide one negative integer and one positive integer in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger 4)]
            division [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc (-2))
    describe "Division with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            division [AstBoolean True, AstInteger 4] [] `shouldBe` (ErrFunc "Error: Division requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            division [AstInteger 3, AstBoolean False] [] `shouldBe` (ErrFunc "Error: Division requires two integer values.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            division [AstSymbol "x", AstInteger 8] [] `shouldBe` (ErrFunc "Error in division 'a': x is not in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not" $ do
            division [AstInteger 8, AstSymbol "y"] [] `shouldBe` (ErrFunc "Error in division 'b': y is not in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not" $ do
            let env = [("x", AstInteger 8)]
            division [AstSymbol "x", AstInteger 0] env `shouldBe` (ErrFunc "Error: Division by 0 is prohibited")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            division [AstBoolean True, AstBoolean False] [] `shouldBe` (ErrFunc "Error: Division requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            division [AstInteger 8] [] `shouldBe` (ErrFunc "Error in division: Insufficient arguments.")
            division [] [] `shouldBe` (ErrFunc "Error in division: Insufficient arguments.")

testModFunction :: Spec
testModFunction = do
    describe "Modulo with valid integer" $ do
        it "should calculate the modulo of two positive integers" $ do
            modulo [AstInteger 8, AstInteger 4] [] `shouldBe` (IntFunc 0)
        it "should calculate the modulo of two negative integers" $ do
            modulo [AstInteger (-8), AstInteger (-4)] [] `shouldBe` (IntFunc 0)
        it "should calculate the modulo of one positive integer and one negative integer" $ do
            modulo [AstInteger 8, AstInteger (-4)] [] `shouldBe` (IntFunc 0)
        it "should calculate the modulo of one negative integer and one positive integer" $ do
            modulo [AstInteger (-8), AstInteger 4] [] `shouldBe` (IntFunc 0)
    describe "Modulo with valid symbol" $ do
        it "should calculate the modulo of two positive integers in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger 4)]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 0)
        it "should calculate the modulo of two negative integers in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger (-4))]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 0)
        it "should calculate the modulo of one positive integer and one negative integer in the environment" $ do
            let env = [("x", AstInteger 8), ("y", AstInteger (-4))]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 0)
        it "should calculate the modulo of one negative integer and one positive integer in the environment" $ do
            let env = [("x", AstInteger (-8)), ("y", AstInteger 4)]
            modulo [AstSymbol "x", AstSymbol "y"] env `shouldBe` (IntFunc 0)
    describe "Modulo with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            modulo [AstBoolean True, AstInteger 4] [] `shouldBe` (ErrFunc "Error: Modulo requires two integer values.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            modulo [AstInteger 3, AstBoolean False] [] `shouldBe` (ErrFunc "Error: Modulo requires two integer values.")
        it "should return an error if 'a' is an integer and 'b' is not in the environment" $ do
            modulo [AstSymbol "x", AstInteger 8] [] `shouldBe` (ErrFunc "Error in modulo 'a': x is not in the environment.")
        it "should return an error if 'a' is an integer and 'b' is not in the environment" $ do
            modulo [AstInteger 8, AstSymbol "y"] [] `shouldBe` (ErrFunc "Error in modulo 'b': y is not in the environment.")
        it "should return an error if 'b' is an integer and 'a' is not in the environment" $ do
            let env = [("x", AstInteger 8)]
            modulo [AstSymbol "y", AstInteger 0] env `shouldBe` (ErrFunc "Error: Modulo by 0 is prohibited")
        it "should return an error if both 'a' and 'b' are not valid integers or symbols" $ do
            modulo [AstBoolean True, AstBoolean False] [] `shouldBe` (ErrFunc "Error: Modulo requires two integer values.")
        it "should return an error if there are insufficient arguments." $ do
            modulo [AstInteger 8] [] `shouldBe` (ErrFunc "Error in modulo: Insufficient arguments.")
            modulo [] [] `shouldBe` (ErrFunc "Error in modulo: Insufficient arguments.")

testInferiorFunction :: Spec
testInferiorFunction = do
    describe "Comparison with valid integer values" $ do
        it "should return True if 'a' is less than 'b'" $ do
            inferior [AstInteger 3, AstInteger 4] [] `shouldBe` (BoolFunc True)
        it "should return False if 'a' is greater than 'b'" $ do
            inferior [AstInteger 5, AstInteger 4] [] `shouldBe` (BoolFunc False)
        it "should return False if 'a' is equal to 'b'" $ do
            inferior [AstInteger 4, AstInteger 4] [] `shouldBe` (BoolFunc False)
    describe "Comparison with valid symbol values" $ do
        it "should return True if symbol 'x' is less than symbol 'y'" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            inferior [AstSymbol "x", AstSymbol "y"] env `shouldBe` (BoolFunc True)
        it "should return False if symbol 'x' is greater than symbol 'y'" $ do
            let env = [("x", AstInteger 5), ("y", AstInteger 4)]
            inferior [AstSymbol "x", AstSymbol "y"] env `shouldBe` (BoolFunc False)
        it "should return False if symbol 'x' is equal to symbol 'y'" $ do
            let env = [("x", AstInteger 4), ("y", AstInteger 4)]
            inferior [AstSymbol "x", AstSymbol "y"] env `shouldBe` (BoolFunc False)
    describe "Comparison with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            inferior [AstBoolean True, AstInteger 4] [] `shouldBe` (ErrFunc "Error: Inferior requires two integer values.")
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            inferior [AstSymbol "x", AstBoolean False] [] `shouldBe` (ErrFunc "Error in inferior 'a': x is not in the environment.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            inferior [AstBoolean False, AstSymbol "y"] [] `shouldBe` (ErrFunc "Error in inferior 'b': y is not in the environment.")
    describe "Insufficient arguments." $ do
        it "should return an error if there are insufficient arguments." $ do
            inferior [AstInteger 3] [] `shouldBe` (ErrFunc "Error in inferior: Insufficient arguments.")
            inferior [] [] `shouldBe` (ErrFunc "Error in inferior: Insufficient arguments.")

testEqualFunction :: Spec
testEqualFunction = do
    describe "Equality with valid integer values" $ do
        it "should return True if 'a' is equal to 'b'" $ do
            equal [AstInteger 4, AstInteger 4] [] `shouldBe` (BoolFunc True)
        it "should return False if 'a' is not equal to 'b'" $ do
            equal [AstInteger 3, AstInteger 4] [] `shouldBe` (BoolFunc False)
    describe "Equality with valid symbol values" $ do
        it "should return True if symbol 'x' is equal to symbol 'y'" $ do
            let env = [("x", AstInteger 4), ("y", AstInteger 4)]
            equal [AstSymbol "x", AstSymbol "y"] env `shouldBe` (BoolFunc True)
        it "should return False if symbol 'x' is not equal to symbol 'y'" $ do
            let env = [("x", AstInteger 3), ("y", AstInteger 4)]
            equal [AstSymbol "x", AstSymbol "y"] env `shouldBe` (BoolFunc False)
    describe "Equality with errors" $ do
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            equal [AstBoolean True, AstInteger 4] [] `shouldBe` (ErrFunc "Error: Equal requires two integer values.")
        it "should return an error if 'a' is not a valid integer or symbol" $ do
            equal [AstSymbol "y", AstBoolean False] [] `shouldBe` (ErrFunc "Error in equal 'a': y is not in the environment.")
        it "should return an error if 'b' is not a valid integer or symbol" $ do
            equal [AstBoolean False, AstSymbol "x"] [] `shouldBe` (ErrFunc "Error in equal 'b': x is not in the environment.")
    describe "Insufficient arguments." $ do
        it "should return an error if there are insufficient arguments." $ do
            equal [AstInteger 3] [] `shouldBe` (ErrFunc "Error in equal: Insufficient arguments.")
            equal [] [] `shouldBe` (ErrFunc "Error in equal: Insufficient arguments.")

testIfFunction :: Spec
testIfFunction = do
    describe "ifFunction" $ do
        it "should return Value x when the condition is #t and the true branch is Value x" $ do
            ifFunction [AstBoolean True, AstInteger 42, AstInteger 24] [] `shouldBe` (IntFunc 42)
        it "should return Value x in env when the condition is #t and the true branch is Value x" $ do
            let env = [("x", AstBoolean True)]
            ifFunction [AstSymbol "x", AstInteger 42, AstInteger 24] env `shouldBe` (IntFunc 42)
        it "should return Bool x when the condition is #t and the true branch is Bool x" $ do
            ifFunction [AstBoolean True, AstBoolean False, AstBoolean True] [] `shouldBe` (BoolFunc False)
        it "should return Err x when the condition is #t and the true branch is Err x" $ do
            ifFunction [AstBoolean True, AstBoolean False, AstSymbol "undefined"] [] `shouldBe` (BoolFunc False)
        it "should return Value y when the condition is #f and the false branch is Value y" $ do
            ifFunction [AstBoolean False, AstInteger 42, AstInteger 24] [] `shouldBe` (IntFunc 24)
        it "should return Bool y when the condition is #f and the false branch is Bool y" $ do
            ifFunction [AstBoolean False, AstBoolean True, AstBoolean False] [] `shouldBe` (BoolFunc False)
        it "should return Err y when the condition is #f and the false branch is Err y" $ do
            ifFunction [AstBoolean False, AstBoolean True, AstSymbol "undefined"] [] `shouldBe` (ErrFunc "undefined is not in the environment.")
        it "should return Err y when the condition is #f and the false branch is Err y" $ do
            ifFunction [AstBoolean True, AstSymbol "undefined", AstBoolean True] [] `shouldBe` (ErrFunc "undefined is not in the environment.")
        it "should return an error when the condition is not a valid boolean" $ do
            ifFunction [AstInteger 1, AstInteger 42, AstInteger 24] [] `shouldBe` (ErrFunc "Error in if: First argument must be a boolean condition.")
        it "should return an error when there are insufficient arguments" $ do
            ifFunction [AstBoolean True, AstInteger 42] [] `shouldBe` (ErrFunc "Error in if: Insufficient arguments.")

testGetSymbol :: Spec
testGetSymbol = do
    describe "getSymbol" $ do
        it "should return IntRes value if the symbol is bound to an integer in the environment" $ do
            let env = [("x", AstInteger 42)]
            getSymbol "x" env `shouldBe` IntRes 42
        it "should return BoolRes value if the symbol is bound to a boolean in the environment" $ do
            let env = [("flag", AstBoolean True)]
            getSymbol "flag" env `shouldBe` BoolRes True
        it "should recursively evaluate the symbol if it is bound to another symbol in the environment" $ do
            let env = [("x", AstSymbol "y"), ("y", AstInteger 99)]
            getSymbol "x" env `shouldBe` IntRes 99
        it "should return ExprRes for functions (AstDefine, AstLambda)" $ do
            let env = [("func", AstDefine (Left "f") (AstLambda ["x"] (AstSymbol "x")))]
            getSymbol "func" env `shouldBe` ExprRes "function define func."
        it "should return ErrRes if the symbol is not in the environment" $ do
            getSymbol "unknown" [] `shouldBe` ErrRes "unknown is not in the environment."
        it "should return ErrRes if the symbol is bound to an unsupported expression type" $ do
            let env = [("unsupported", AstLambda ["x"] (AstSymbol "x"))]
            getSymbol "unsupported" env `shouldBe` ExprRes "function lambda unsupported."

testAddOrReplaceKey :: Spec
testAddOrReplaceKey = do
    describe "addOrReplaceKey" $ do
        it "should add a new key-value pair to an empty environment" $ do
            addOrReplaceKey "x" (AstInteger 42) [] `shouldBe` [("x", AstInteger 42)]
        it "should replace the value of an existing key" $ do
            let env = [("x", AstInteger 42)]
            addOrReplaceKey "x" (AstInteger 24) env `shouldBe` [("x", AstInteger 24)]
        it "should add a new key-value pair to an existing environment" $ do
            let env = [("y", AstBoolean True)]
            addOrReplaceKey "x" (AstInteger 42) env `shouldBe` [("x", AstInteger 42), ("y", AstBoolean True)]

testHandleFunctionBody :: Spec
testHandleFunctionBody = do
    describe "handleFunctionBody" $ do
        it "should handle IntRes result" $ do
            let env = [("a", AstInteger 5)]
            handleFunctionBody "b" (AstInteger 10) env `shouldBe` EnvRes [("b", AstInteger 10), ("a", AstInteger 5)]
        it "should handle BoolRes result" $ do
            let env = [("a", AstBoolean True)]
            handleFunctionBody "b" (AstBoolean False) env `shouldBe` EnvRes [("b", AstBoolean False), ("a", AstBoolean True)]
        it "should handle ExprRes result" $ do
            let env = [("a", AstInteger 5)]
            handleFunctionBody "b" (AstSymbol "a") env `shouldBe` EnvRes [("b", AstInteger 5), ("a", AstInteger 5)]
        it "should handle invalid assignment to function" $ do
            let env = [("a", AstDefine (Right ["x"]) (AstSymbol "x"))]
            handleFunctionBody "a" (AstInteger 10) env `shouldBe` EnvRes [("a",AstInteger 10)]

testHandleDefinedSymbolName :: Spec
testHandleDefinedSymbolName = do
    describe "handleDefinedSymbolName" $ do
        it "should handle defined symbol name with parameters" $ do
            let env = [("a", AstInteger 5)]
            handleDefinedSymbolName "b" ["x"] (AstSymbol "x") env `shouldBe` EnvRes [("b", AstDefine (Right ["x"]) (AstSymbol "x")), ("a", AstInteger 5)]
        it "should handle defined symbol name without parameters" $ do
            let env = [("a", AstInteger 5)]
            handleDefinedSymbolName "b" [] (AstInteger 10) env `shouldBe` EnvRes [("b", AstDefine (Right []) (AstInteger 10)), ("a", AstInteger 5)]

testDefineSymbol :: Spec
testDefineSymbol = do
    describe "defineSymbol" $ do
        it "should handle Left case with valid body" $ do
            let env = [("a", AstInteger 5)]
            defineSymbol (Left "b") (AstInteger 10) env `shouldBe` EnvRes [("b", AstInteger 10), ("a", AstInteger 5)]
        it "should handle Left case with invalid assignment" $ do
            let env = [("a", AstDefine (Right ["x"]) (AstSymbol "x"))]
            defineSymbol (Left "a") (AstInteger 10) env `shouldBe` EnvRes [("a",AstInteger 10)]
        it "should handle Right case with empty parameters" $ do
            let env = [("a", AstInteger 5)]
            defineSymbol (Right []) (AstInteger 10) env `shouldBe` ErrRes "Symbol name is not defined."
        it "should handle Right case with valid parameters" $ do
            let env = [("a", AstInteger 5)]
            defineSymbol (Right ["b"]) (AstSymbol "x") env `shouldBe` EnvRes [("b", AstDefine (Right []) (AstSymbol "x")), ("a", AstInteger 5)]
        it "should handle Left case with function assignment" $ do
            let env = [("a", AstDefine (Right ["x"]) (AstSymbol "x"))]
            defineSymbol (Left "a") (AstDefine (Right ["y"]) (AstSymbol "y")) env `shouldBe` ErrRes "Cannot assign to symbol 'a'. Functions cannot be assigned directly."


testAddKeyVal :: Spec
testAddKeyVal = do
    describe "addKeyVal" $ do
        it "should add a key-value pair to an empty environment" $ do
            let env = []
            addKeyVal "a" (AstInteger 42) env `shouldBe` [("a", AstInteger 42)]
        it "should add a key-value pair to a non-empty environment" $ do
            let env = [("x", AstBoolean True), ("y", AstSymbol "foo")]
            addKeyVal "z" (AstInteger 123) env `shouldBe` [("z", AstInteger 123), ("x", AstBoolean True), ("y", AstSymbol "foo")]
        it "should replace the value if the key already exists" $ do
            let env = [("x", AstBoolean True), ("y", AstSymbol "foo")]
            addKeyVal "y" (AstInteger 456) env `shouldBe` [("y", AstInteger 456), ("x", AstBoolean True), ("y", AstSymbol "foo")]
        it "should handle adding a key-value pair with various types of values" $ do
            let env = [("a", AstBoolean False), ("b", AstInteger 789)]
            addKeyVal "c" (AstSymbol "bar") env `shouldBe` [("c", AstSymbol "bar"), ("a", AstBoolean False), ("b", AstInteger 789)]
        it "should handle adding multiple key-value pairs" $ do
            let env = [("x", AstBoolean True)]
            addKeyVal "y" (AstInteger 999) (addKeyVal "z" (AstSymbol "test") env) `shouldBe` [("y", AstInteger 999), ("z", AstSymbol "test"), ("x", AstBoolean True)]

testSetFunctionInEnv :: Spec
testSetFunctionInEnv = do
    describe "setFunctionInEnv" $ do
        it "should return Left env for empty lists" $ do
            let env = [("x", AstBoolean True)]
            setFunctionInEnv [] [] env `shouldBe` Left env
        it "should return Right 'Too few arguments' for non-empty parameter names and empty argument list" $ do
            setFunctionInEnv ["x", "y"] [] [("z", AstInteger 42)] `shouldBe` Right "Too few arguments"
        it "should return Right 'Too many arguments' for empty parameter names and non-empty argument list" $ do
            setFunctionInEnv [] [AstInteger 10, AstBoolean True] [("z", AstInteger 42)] `shouldBe` Right "Too many arguments"
        it "should set integer value in the environment" $ do
            let env = [("x", AstBoolean True)]
            setFunctionInEnv ["y"] [AstInteger 42] env `shouldBe` Left [("y", AstInteger 42), ("x", AstBoolean True)]
        it "should set boolean value in the environment" $ do
            let env = [("x", AstInteger 5)]
            setFunctionInEnv ["y"] [AstBoolean False] env `shouldBe` Left [("y", AstBoolean False), ("x", AstInteger 5)]
        it "should return Right error message for non-integer/boolean result" $ do
            let env = [("x", AstBoolean True)]
            setFunctionInEnv ["y"] [AstSymbol "foo"] env `shouldBe` Right "foo is not in the environment."
        it "should handle multiple parameters and arguments" $ do
            let env = [("a", AstInteger 1), ("b", AstBoolean True)]
            setFunctionInEnv ["x", "y"] [AstInteger 42, AstBoolean False] env `shouldBe` Left [("y", AstBoolean False), ("x", AstInteger 42), ("a", AstInteger 1), ("b", AstBoolean True)]
        it "should return Right error message for an error result" $ do
            let env = [("x", AstBoolean True)]
            setFunctionInEnv ["y"] [AstSymbol "Invalid expression"] env `shouldBe` Right "Invalid expression is not in the environment."

testCallFunction :: Spec
testCallFunction = do
    describe "callFunction" $ do
        it "should return ErrFunc for incorrect number of arguments" $ do
            let env = [("x", AstInteger 5)]
            callFunction "func" ["x", "y"] (AstSymbol "y") [AstBoolean False] env `shouldBe` ErrFunc "Calling func with an incorrect number of arguments."
        it "should return IntFunc for correct return type" $ do
            let env = [("x", AstInteger 5), ("y", AstBoolean True)]
            callFunction "func" ["x", "y"] (AstSymbol "y") [AstBoolean False, AstInteger 10] env `shouldBe` IntFunc 10
        it "should handle correct function call" $ do
            let env = [("x", AstInteger 5), ("y", AstBoolean True)]
            callFunction "func" ["x", "y"] (AstSymbol "y") [AstBoolean False, AstInteger 10] env `shouldBe` IntFunc 10

testFuncCall :: Spec
testFuncCall = do
    describe "funcCall" $ do
        it "should return ErrFunc for invalid call" $ do
            let env = [("x", AstInteger 5)]
            funcCall "func" (AstInteger 10) [AstBoolean True] env `shouldBe` ErrFunc "Invalid call func."
        it "should call callFunction for AstDefine" $ do
            let env = [("x", AstInteger 5)]
            funcCall "func" (AstDefine (Right ["x"]) (AstSymbol "x")) [AstBoolean True] env `shouldBe` BoolFunc True
        it "should call callFunction for AstLambda" $ do
            let env = [("x", AstInteger 5)]
            funcCall "func" (AstLambda ["x"] (AstSymbol "x")) [AstBoolean True] env `shouldBe` BoolFunc True


testCallFunc :: Spec
testCallFunc = do
    describe "callFunc" $ do
        it "should return ErrFunc for incorrect number of arguments in lambda" $ do
            let env = [("x", AstInteger 5)]
            callFunc [AstLambda ["x"] (AstSymbol "x")] env `shouldBe` ErrFunc "Calling lambda with an incorrect number of arguments."
        it "should return ErrFunc for invalid symbol" $ do
            let env = [("x", AstInteger 5)]
            callFunc [AstSymbol "func"] env `shouldBe` ErrFunc "func is not in the environment."
        it "should call funcCall for valid symbol" $ do
            let env = [("func", AstLambda ["x"] (AstSymbol "x"))]
            callFunc [AstSymbol "func", AstBoolean True] env `shouldBe` BoolFunc True


testIsBuild :: Spec
testIsBuild = do
    describe "isBuild" $ do
        it "should return Right IntFunc for valid '+' function" $ do
            let env = [("x", AstInteger 5), ("y", AstInteger 10)]
            isBuild [AstSymbol "+", AstSymbol "x", AstSymbol "y"] env `shouldBe` Right (IntFunc 15)
        it "should return Left 'Function not found' for invalid function" $ do
            isBuild [AstSymbol "unknown"] [] `shouldBe` Left "Function not found"
        it "should return Error 'Invalid function call.' for invalid function" $ do
            isBuild [AstInteger 1] [] `shouldBe` Left "Invalid function call."

testCallUserFunction :: Spec
testCallUserFunction = do
    describe "callUserFunction" $ do
        it "should return IntRes for valid integer result" $ do
            let env = [("x", AstInteger 5)]
            callUserFunction [AstSymbol "x"] env `shouldBe` ErrRes "Invalid call x."
        it "should return BoolRes for valid boolean result" $ do
            let env = [("x", AstBoolean True)]
            callUserFunction [AstSymbol "x"] env `shouldBe` ErrRes "Invalid call x."
        it "should return ErrRes for invalid function call" $ do
            let env = [("x", AstInteger 5)]
            callUserFunction [AstSymbol "unknown"] env `shouldBe` ErrRes "unknown is not in the environment."

testCallBuildFunction :: Spec
testCallBuildFunction = do
    describe "callBuildFunction" $ do
        it "should return IntRes for valid integer result" $ do
            let env = [("x", AstInteger 5)]
            callBuildFunction [AstSymbol "x"] env `shouldBe` ErrRes "Invalid call x."
        it "should return BoolRes for valid boolean result" $ do
            let env = [("x", AstBoolean True)]
            callBuildFunction [AstSymbol "x"] env `shouldBe` ErrRes "Invalid call x."
        it "should return ErrRes for invalid function call" $ do
            let env = [("x", AstInteger 5)]
            callBuildFunction [AstSymbol "unknown"] env `shouldBe` ErrRes "unknown is not in the environment."

testFunctionValue :: Spec
testFunctionValue = do
    describe "functionValue" $ do
        it "should return IntRes for valid integer result" $ do
            let env = [("x", AstInteger 5)]
            functionValue [AstSymbol "x"] env `shouldBe` ErrRes "Invalid call x."
        it "should return BoolRes for valid boolean result" $ do
            let env = [("x", AstBoolean True)]
            functionValue [AstSymbol "x"] env `shouldBe` ErrRes "Invalid call x."
        it "should return ErrRes for invalid function call" $ do
            let env = [("x", AstInteger 5)]
            functionValue [AstSymbol "unknown"] env `shouldBe` ErrRes "unknown is not in the environment."

testEval :: Spec
testEval = do
    describe "eval" $ do
        it "should return IntRes for AstInteger" $ do
            eval (AstInteger 42) [] `shouldBe` IntRes 42
        it "should return BoolRes for AstBoolean" $ do
            eval (AstBoolean True) [] `shouldBe` BoolRes True
        it "should call getSymbol for AstSymbol" $ do
            let env = [("x", AstInteger 10)]
            eval (AstSymbol "x") env `shouldBe` IntRes 10
        it "should call functionValue for AstCall" $ do
            let env = [("add", AstLambda ["x", "y"] (AstCall [AstSymbol "+", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "add", AstInteger 5, AstInteger 7]) env `shouldBe` IntRes 12
        it "should call isBuild for AstCall with built-in functions" $ do
            eval (AstCall [AstSymbol "+", AstInteger 3, AstInteger 5]) [] `shouldBe` IntRes 8
        it "should call defineSymbol for AstDefine" $ do
            eval (AstDefine (Left "x") (AstInteger 42)) [] `shouldBe` EnvRes [("x", AstInteger 42)]
        it "should call defineSymbol for AstDefine with lambda" $ do
            eval (AstDefine (Left "add") (AstLambda ["x", "y"] (AstCall [AstSymbol "+", AstSymbol "x", AstSymbol "y"]))) [] `shouldBe` EnvRes [("add", AstLambda ["x", "y"] (AstCall [AstSymbol "+", AstSymbol "x", AstSymbol "y"]))]
        it "should return ExprRes for AstLambda" $ do
            eval (AstLambda ["x", "y"] (AstCall [AstSymbol "+", AstSymbol "x", AstSymbol "y"])) [] `shouldBe` ExprRes "lambda"

testEvalBasicFunc :: Spec
testEvalBasicFunc = do
    describe "eval basic function" $ do
        it "should call add with myAdd function 2 positive int" $ do
            let env = [("myAdd", AstLambda ["x", "y"] (AstCall [AstSymbol "+", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "myAdd", AstInteger 5, AstInteger 7]) env `shouldBe` IntRes 12
        it "should call add with mySub function 2 positive int" $ do
            let env = [("mySub", AstLambda ["x", "y"] (AstCall [AstSymbol "-", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "mySub", AstInteger 5, AstInteger 7]) env `shouldBe` IntRes (-2)
        it "should call add with myMult function 2 positive int" $ do
            let env = [("myMult", AstLambda ["x", "y"] (AstCall [AstSymbol "*", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "myMult", AstInteger 7, AstInteger 5]) env `shouldBe` IntRes 35
        it "should call add with myDiv function 2 positive int" $ do
            let env = [("myDiv", AstLambda ["x", "y"] (AstCall [AstSymbol "div", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "myDiv", AstInteger 10, AstInteger 2]) env `shouldBe` IntRes 5
        it "should call add with myMod function 2 positive int" $ do
            let env = [("myMod", AstLambda ["x", "y"] (AstCall [AstSymbol "mod", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "myMod", AstInteger 7, AstInteger 5]) env `shouldBe` IntRes 2
        it "should evaluate ifFunction with true condition" $ do
            let env = [("myIf", AstLambda ["condition", "thenExpr", "elseExpr"] (AstCall [AstSymbol "if", AstSymbol "condition", AstSymbol "thenExpr", AstSymbol "elseExpr"]))]
            eval (AstCall [AstSymbol "myIf", AstBoolean True, AstInteger 42, AstInteger 0]) env `shouldBe` IntRes 42
        it "should evaluate ifFunction with false condition" $ do
            let env = [("myIf", AstLambda ["condition", "thenExpr", "elseExpr"] (AstCall [AstSymbol "if", AstSymbol "condition", AstSymbol "thenExpr", AstSymbol "elseExpr"]))]
            eval (AstCall [AstSymbol "myIf", AstBoolean False, AstInteger 42, AstInteger 0]) env `shouldBe` IntRes 0
        it "should evaluate inferior function" $ do
            let env = [("myLessThan", AstLambda ["x", "y"] (AstCall [AstSymbol "<", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "myLessThan", AstInteger 5, AstInteger 7]) env `shouldBe` BoolRes True
            eval (AstCall [AstSymbol "myLessThan", AstInteger 7, AstInteger 5]) env `shouldBe` BoolRes False
        it "should evaluate equal function" $ do
            let env = [("myEqual", AstLambda ["x", "y"] (AstCall [AstSymbol "eq?", AstSymbol "x", AstSymbol "y"]))]
            eval (AstCall [AstSymbol "myEqual", AstInteger 5, AstInteger 5]) env `shouldBe` BoolRes True
            eval (AstCall [AstSymbol "myEqual", AstInteger 5, AstInteger 7]) env `shouldBe` BoolRes False



spec :: Spec
spec = do
    -- Check Mandatory function
    testAddFunction
    testSubFunction
    testMultFunction
    testDivFunction
    testModFunction
    testInferiorFunction
    testEqualFunction
    testIfFunction

    -- Check AstDefine
    testGetSymbol
    testAddOrReplaceKey
    testHandleFunctionBody
    testHandleDefinedSymbolName
    testDefineSymbol

    -- Check AstCall user function
    testAddKeyVal
    testSetFunctionInEnv
    testCallFunction
    testFuncCall
    testCallFunc

    -- Check AstCall user function
    testIsBuild
    testCallUserFunction
    testCallBuildFunction
    testFunctionValue

    -- Check Eval (main function of Evaluation)
    testEval

    -- Check all basic function from isBuild function
    testEvalBasicFunc