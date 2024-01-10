module AstSpec (spec) where

import Test.Hspec
import Ast
import Cpt

testGetSymbol :: Spec
testGetSymbol = do
    describe "getSymbol" $ do
        it "returns Just the symbol from Cpt" $
            getSymbol (CptSymbols "x") `shouldBe` Just "x"
        it "returns Nothing for non-symbol Cpt" $
            getSymbol (CptInt 42) `shouldBe` Nothing

testGetInteger :: Spec
testGetInteger = do
    describe "getInteger" $ do
        it "returns Just the integer from Cpt" $
            getInteger (CptInt 42) `shouldBe` Just 42
        it "returns Nothing for non-integer Cpt" $
            getInteger (CptSymbols "x") `shouldBe` Nothing

testGetList :: Spec
testGetList = do
    describe "getList" $ do
        it "returns Just the list from Cpt" $
            getList (CptList [CptSymbols "x", CptInt 42]) `shouldBe` Just [CptSymbols "x", CptInt 42]
        it "returns Nothing for non-list Cpt" $
            getList (CptInt 42) `shouldBe` Nothing

testCptToAST :: Spec
testCptToAST = do
    describe "cptToAST" $ do
        it "converts CptInt to AstInteger" $
            cptToAST (CptInt 42) `shouldBe` Just (AstInteger 42)
        it "converts CptSymbols to AstSymbol" $
            cptToAST (CptSymbols "define") `shouldBe` Just (AstSymbol "define")
        it "handles empty list" $
            cptToAST (CptList []) `shouldBe` Nothing
        it "converts define expression with a single value" $
            cptToAST (CptList [CptSymbols "define", CptSymbols "x", CptInt 42])
                `shouldBe` Just (AstDefine (Left "x") (AstInteger 42))
        it "handles invalid define expression -> multiple values" $
            cptToAST (CptList [CptSymbols "define", CptSymbols "x", CptInt 42, CptSymbols "y"])
                `shouldBe` Nothing
        it "converts function call expression" $
            cptToAST (CptList [CptSymbols "add", CptInt 1, CptInt 2])
                `shouldBe` Just (AstCall [AstSymbol "add", AstInteger 1, AstInteger 2])
        it "handles empty function call expression" $
            cptToAST (CptList [CptSymbols "add"])
                `shouldBe` Nothing
        it "handles nested function call expression" $
            cptToAST (CptList [CptSymbols "add", CptInt 1, CptList [CptSymbols "multiply", CptInt 2, CptInt 3]])
                `shouldBe` Just (AstCall [AstSymbol "add", AstInteger 1, AstCall [AstSymbol "multiply", AstInteger 2, AstInteger 3]])

testCptListToAst :: Spec
testCptListToAst = do
    describe "cptListToAst" $ do
        it "handles empty list (Nothing)" $
            cptListToAst [] `shouldBe` Nothing
        it "converts a single Cpt expression to Ast" $
            cptListToAst [CptInt 42] `shouldBe` Just (AstInteger 42)
        it "handles nested calls in the list" $
            cptListToAst [CptList [CptSymbols "add", CptInt 1, CptList [CptSymbols "multiply", CptInt 2, CptInt 3]]] `shouldBe`
                Just (AstCall [AstSymbol "add", AstInteger 1, AstCall [AstSymbol "multiply", AstInteger 2, AstInteger 3]])

spec :: Spec
spec = do
    testGetSymbol
    testGetInteger
    testGetList
    testCptToAST
    testCptListToAst
