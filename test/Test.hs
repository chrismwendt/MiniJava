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

main
  = defaultMain
  $ testGroup "Unit tests"
  $ map (testCase "Compile" . uncurry testCode) successExamples

runProgram program = bracket (writeFile "program.mips" program) (const $ removeFile "program.mips") $ \_ -> do
    (_, output, _) <- readProcessWithExitCode "spim" ["-file", "program.mips"] ""
    return output

testCode sourceFile expectedOutputFile = do
    expected <- readFile expectedOutputFile
    actual <- (unlines . drop 1 . lines) <$> (runProgram =<< atCode <$> readFile sourceFile)
    actual @?= expected

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
    [ "examples/Fail10.java"
    , "examples/Fail1.java"
    , "examples/Fail2.java"
    , "examples/Fail4.java"
    , "examples/Fail11.java"
    , "examples/Fail20.java"
    , "examples/Fail30.java"
    , "examples/Fail5.java"
    , "examples/Fail12.java"
    , "examples/Fail21.java"
    , "examples/Fail31.java"
    , "examples/Fail6.java"
    , "examples/Fail13.java"
    , "examples/Fail22.java"
    , "examples/Fail32.java"
    , "examples/Fail7.java"
    , "examples/Fail14.java"
    , "examples/Fail24.java"
    , "examples/Fail33.java"
    , "examples/Fail8.java"
    , "examples/Fail15.java"
    , "examples/Fail25.java"
    , "examples/Fail34.java"
    , "examples/Fail9.java"
    , "examples/Fail16.java"
    , "examples/Fail26.java"
    , "examples/Fail35.java"
    , "examples/Fail17.java"
    , "examples/Fail28.java"
    , "examples/Fail36.java"
    , "examples/Fail18.java"
    , "examples/Fail29.java"
    , "examples/Fail3.java"
    ]
