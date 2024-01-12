module TokenSpec (spec) where

import Test.Hspec
import Tokenization



testTokenizeString :: Spec
testTokenizeString = do
    describe "Tokenize with valid string" $ do
        it "Tokenize simple line" $ do
            let string = "define x 3;"
            stringToToken string `shouldBe` (["define", "x", "3", ")"])
        it "Tokenize simple line with \\n" $ do
            let string = "define foo 21;\n* foo 2;"
            stringToToken string `shouldBe` (["define", "foo", "21", ")", "(", "*", "foo", "2", ")"])
        it "Tokenize complex line with \\n" $ do
            let string = "define add (lambda (a b) (+ a b));\nadd 3 4;"
            stringToToken string `shouldBe` (["define", "add", "(", "lambda", "(", "a", "b", ")", "(", "+", "a", "b", ")", ")", ")", "(", "add", "3", "4", ")"])

testCheckParenthesis :: Spec
testCheckParenthesis = do
    describe "Check Parentheses" $ do
        it "Empty input" $ do
            checkParenthesis [] `shouldBe` (0, 0)
        it "Single opening parenthesis" $ do
            checkParenthesis ["("] `shouldBe` (1, 0)
        it "Single closing parenthesis" $ do
            checkParenthesis [")"] `shouldBe` (0, 1)
        it "Balanced parentheses" $ do
            checkParenthesis ["(", ")", "(", ")"] `shouldBe` (2, 2)
        it "Unbalanced parentheses" $ do
            checkParenthesis ["(", "(", ")"] `shouldBe` (2, 1)


spec :: Spec
spec = do
    testTokenizeString
    testCheckParenthesis