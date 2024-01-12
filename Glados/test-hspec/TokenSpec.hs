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

spec :: Spec
spec = do
    testTokenizeString