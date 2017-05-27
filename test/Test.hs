{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.Process
import System.Directory
import System.Exit
import Control.Monad.Loops
import Control.Exception
import System.Exit
import Control.Monad
import Data.List
import SSACompiler
import Test.Tasty
import Test.Tasty.HUnit
import Interface
import Control.Monad.Except

main
  = defaultMain
  $ testGroup "Unit tests"
  $ map (testCase "Compile" . uncurry testCode) successExamples
    ++ map (testCase "Fail" . testFail) failExamples

runProgram program = bracket (writeFile "program.mips" program) (const $ removeFile "program.mips") $ \_ -> do
  (e, output, _) <- readProcessWithExitCode "spim" ["-file", "program.mips"] ""
  return (e, output)

runProgramOut program = do
  (_, o) <- runProgram program
  return o

testCode sourceFile expectedOutputFile = do
  expected <- readFile expectedOutputFile
  actual <- (unlines . drop 1 . lines) <$> (runProgramOut =<< (either id id . runExcept) . atCode <$> readFile sourceFile)
  actual @?= expected

testFail sourceFile = do
  r <- runExcept . atCode <$> readFile sourceFile
  case r of
    Right _ -> assertFailure "Expected failure"
    Left _ -> return ()

successExamples =
  [ ("examples/Leet.java", "examples/Leet.output")
  , ("examples/Spill.java", "examples/Spill.output")
  , ("examples/Call.java", "examples/Call.output")
  , ("examples/BinarySearch.java", "examples/BinarySearch.output")
  , ("examples/BinaryTree.java", "examples/BinaryTree.output")
  , ("examples/StackArgs.java", "examples/StackArgs.output")
  , ("examples/TreeVisitor.java", "examples/TreeVisitor.output")
  , ("examples/Loops.java", "examples/Loops.output")
  , ("examples/Fields.java", "examples/Fields.output")
  , ("examples/CallThis.java", "examples/CallThis.output")
  , ("examples/Extends.java", "examples/Extends.output")
  , ("examples/Arg.java", "examples/Arg.output")
  , ("examples/LinearSearch.java", "examples/LinearSearch.output")
  , ("examples/Arithmetic.java", "examples/Arithmetic.output")
  , ("examples/LinkedList.java", "examples/LinkedList.output")
  , ("examples/BubbleSort.java", "examples/BubbleSort.output")
  , ("examples/Min.java", "examples/Min.output")
  , ("examples/Factorial.java", "examples/Factorial.output")
  , ("examples/Comparisons.java", "examples/Comparisons.output")
  , ("examples/QuickSort.java", "examples/QuickSort.output")
  , ("examples/Add.java", "examples/Add.output")
  ]

failExamples =
  [ "examples/Fail1.java"
  ]
