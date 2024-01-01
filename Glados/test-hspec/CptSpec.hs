module CptSpec (spec) where

import Test.Hspec
import Cpt

testParseElement :: Spec
testParseElement = do
    describe "parseElement" $ do
        it "parses an integer" $
            parseElement ["42"] `shouldBe` (CptInt 42, [])
        it "parses a negative integer" $
            parseElement ["-42"] `shouldBe` (CptInt (-42), [])
        it "parses a symbol" $
            parseElement ["define"] `shouldBe` (CptSymbols "define", [])
        it "parses an empty list" $
            parseElement ["(", ")"] `shouldBe` (CptList [], [])
        it "parses a list with one element" $
            parseElement ["(", "x", "42", ")"] `shouldBe` (CptList [CptSymbols "x", CptInt 42], [])
        it "parses nested lists" $
            parseElement ["(", "define", "(", "x", "42", ")", ")"] `shouldBe` (CptList [CptSymbols "define", CptList [CptSymbols "x", CptInt 42]], [])

testParseList :: Spec
testParseList = do
    describe "parseList" $ do
        it "parses an empty list" $
            parseList [] `shouldBe` ([], [])
        it "ignores closing parenthesis at the end" $
            parseList [")"] `shouldBe` ([], [])
        it "parses a list with one element" $
            parseList ["42"] `shouldBe` ([CptInt 42], [])
        it "parses a list with multiple elements" $
            parseList ["(", "define", "x", "42", ")", "(", "define", "y", "43", ")"] `shouldBe`
                ([CptList [CptSymbols "define", CptSymbols "x", CptInt 42], CptList [CptSymbols "define", CptSymbols "y", CptInt 43]], [])
        it "parses nested lists" $
            parseList ["(", "define", "(", "x", "42", ")", ")", "(", "define", "y", "43", ")"] `shouldBe`
                ([CptList [CptSymbols "define", CptList [CptSymbols "x", CptInt 42]], CptList [CptSymbols "define", CptSymbols "y", CptInt 43]], [])
        -- it "handles incorrect syntax - missing opening parenthesis" $
        --     parseList ["define", "x", "42", ")"] `shouldBe` ([], ["define", "x", "42", ")"])
        -- it "handles incorrect syntax - missing closing parenthesis" $
        --     parseList ["(", "define", "x", "42"] `shouldBe` ([], ["define", "x", "42"])

testTokenToCpt :: Spec
testTokenToCpt = do
    describe "tokenToCpt" $ do
        it "parses an empty input" $
            tokenToCpt [] `shouldBe` []
        it "parses a list with one element" $
            tokenToCpt ["42"] `shouldBe` [CptInt 42]
        it "parses a list with multiple elements" $
            tokenToCpt ["(", "define", "x", "42", ")", "(", "define", "y", "43", ")"] `shouldBe`
                [CptList [CptSymbols "define", CptSymbols "x", CptInt 42], CptList [CptSymbols "define", CptSymbols "y", CptInt 43]]
        -- it "handles incorrect syntax - missing closing parenthesis" $
        --     tokenToCpt ["(", "define", "x", "42"] `shouldBe` []
        -- it "handles incorrect syntax - extra closing parenthesis" $
        --     tokenToCpt ["(", "define", "x", "42", ")", ")"] `shouldBe` []

spec :: Spec
spec = do
    testParseElement
    testParseList
    testTokenToCpt
