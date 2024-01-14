module VmSpec (spec) where

import Test.Hspec
import Vm (
  Value(..),
  performOperation,
  Instruction(..),
  exec,
  parseInstruction,
  Op(..),
  factOp,
  divOp,
  subOp
  )

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

  describe "Additional Perform Operation Tests" $ do
    it "Custom Binary Operation" $
      performOperation (\a b -> a * b + 1) [VInt 2, VInt 3] `shouldBe` Right [VInt 7]

    it "Custom Unary Operation" $
      performOperation (\a _ -> a * 2) [VInt 3] `shouldBe` Left "Error: Invalid arguments on stack"

    it "Invalid Binary Operation Args" $
      performOperation (*) [VBool True, VInt 3] `shouldBe` Left "Error: Invalid arguments on stack"

  describe "Vm Tests" $ do
    it "Executes Push and Pop Instructions" $
      let instructions = [Push (VInt 42), Pop]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Right []

    it "Executes Call Instruction with Unary Operation" $
      let instructions = [Push (VInt 5), Push (VOp Fact), Call]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Right [VInt 120]

    it "Handles Invalid Binary Operation Arguments" $
      let instructions = [Push (VBool True), Push (VInt 3), Push (VOp Mul), Call]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Left "Error: Invalid arguments for binary operation"

    it "Handles JumpIfFalse Instruction" $
      let instructions = [Push (VBool False), JumpIfFalse 2, Push (VInt 42), Push (VInt 99)]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Right [VInt 99]

    it "Handles PushEnv and Define Instructions" $
      let instructions = [Define, PushEnv "x", Push (VInt 42), PushEnv "x"]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Right [VInt 42,VInt 42,VInt 42]

    it "Executes a series of instructions 1" $
      let instructions = [Push (VInt 5), Push (VInt 2), Push (VOp Add), Push (VInt 3), Push (VOp Mul), Call]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Left "Error: Invalid arguments for binary operation"

    it "Executes a series of instructions 2" $
      let instructions = [Push (VInt 5), Push (VInt 2), Push (VOp Less), JumpIfFalse 2, Push (VInt 99), Push (VInt 42)]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Right [VInt 42,VInt 99,VOp Less,VInt 2,VInt 5]

    it "Handles PushEnv with Function and No Arguments" $
      let instructions = [Define, PushEnv "x", Push (VFunc [PushArg 0, Push (VInt 5), Push (VInt 2), Push (VOp Mul), Call]), PushEnv "x", Call]
          initialState = []
          initialArgs = []
          initialEnv = []
      in exec instructions initialState initialArgs initialEnv 0 `shouldBe` Left "Error: Argument index out of bounds"
