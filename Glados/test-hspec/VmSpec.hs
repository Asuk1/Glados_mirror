module VmSpec (spec) where

import Test.Hspec
import Vm (Op(..), Value(..), opToFunction, performOperation, executeInstruction, exec, Instruction(..), Env)

sampleEnv :: [(String, Value)]
sampleEnv = [("var1", VInt 42), ("var2", VBool True)]

spec :: Spec
spec = do
  describe "Op to Function Tests" $ do
    it "Add" $ do
      let opFunction = opToFunction Add
      performBinaryOperation opFunction 2 3 `shouldBe` 5
    it "Sub" $ do
      let opFunction = opToFunction Sub
      performBinaryOperation opFunction 5 2 `shouldBe` (-3)
    it "Mul" $ do
      let opFunction = opToFunction Mul
      performBinaryOperation opFunction 2 3 `shouldBe` 6
    it "Div" $ do
      let opFunction = opToFunction Div
      performBinaryOperation opFunction 6 2 `shouldBe` 3

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

  describe "Execute Instruction Tests" $ do
    it "Push" $
      executeInstruction (Push (VInt 42)) [] [] sampleEnv `shouldBe` Right ([VInt 42], 1)
    it "Pop" $
      executeInstruction Pop [] [] sampleEnv `shouldBe` Left "Error: Not enough arguments on stack"
    it "Call Add" $
      executeInstruction Call [VOp Add, VInt 2, VInt 3] [] sampleEnv `shouldBe` Right ([VInt 5], 1)

  describe "Exec Tests" $ do
    it "Simple Program" $
      exec simpleCode [] [] sampleEnv 0 `shouldBe` Right [VInt 45]

performBinaryOperation :: Either a (Int -> Int -> Int) -> Int -> Int -> Int
performBinaryOperation opFunction a b = case opFunction of
  Right binaryOp -> binaryOp a b
  _ -> error "Unexpected unary operation"

simpleCode :: [Instruction]
simpleCode =
  [ Push (VInt 42)
  , Push (VInt 3)
  , Push (VOp Add)
  , Call
  , Ret
  ]
