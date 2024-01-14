module VmSpec (spec) where

import Test.Hspec
import Vm (Value(..), performOperation, Instruction(..))

spec :: Spec
spec = do
  describe "Perform Operation Tests" $ do
    it "Add" $
      performOperation (+) [VInt 2, VInt 3] `shouldBe` Right [VInt 5]
    it "Sub" $
      performOperation (-) [VInt 5, VInt 2] `shouldBe` Right [VInt 3]
    it "Mul" $
      performOperation (*) [VInt 2, VInt 3] `shouldBe` Right [VInt 6]
    it "Div" $
      performOperation div [VInt 6, VInt 2] `shouldBe` Right [VInt 3]
    it "Invalid Args" $
      performOperation (+) [] `shouldBe` Left "Error: Not enough arguments on stack"