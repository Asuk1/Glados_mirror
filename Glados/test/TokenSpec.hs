module TokenSpec (spec) where

import Test.Hspec
import Tokenization
import Lib



testTokenizeString :: Spec
testTokenizeString = do
    describe "Tokenize with valid string" $ do
        it "Tokenize simple line" $ do
            let string = "(define x 3)"
            stringToToken string `shouldBe` (["(", "define", "x", "3", ")"])
        it "Tokenize simple line with \\n" $ do
            let string = "(define foo 21)\n(* foo 2)"
            stringToToken string `shouldBe` (["(", "define", "foo", "21", ")", "(", "*", "foo", "2", ")"])



spec :: Spec
spec = do
    testTokenizeString