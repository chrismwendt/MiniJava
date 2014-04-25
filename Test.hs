import System.Environment
import System.Process
import Control.Monad.Loops
import System.Exit
import Control.Monad

main = allM test examples >> return ()
-- main = allM test ["examples/Spill.java"] >> return ()

test file = do
    putStrLn $ "Testing " ++ file
    system $ "canonical/bin/mjlex " ++ file ++ " > canonical.out"
    system $ "runhaskell Main.hs --stopAt lex " ++ file ++ " > mine.out"
    (exit, stdout, stderr) <- readProcessWithExitCode "diff" ["-y", "canonical.out", "mine.out"] ""
    case exit of
        ExitFailure _ -> putStr stdout >> return False
        ExitSuccess -> return True

examples =
    [ "examples/Leet.java"
    , "examples/Spill.java"
    , "examples/Call.java"
    , "examples/BinarySearch.java"
    , "examples/BinaryTree.java"
    , "examples/LogicAnd.java"
    , "examples/StackArgs.java"
    , "examples/TreeVisitor.java"
    , "examples/Loops.java"
    , "examples/Fields.java"
    , "examples/CallThis.java"
    , "examples/Extends.java"
    , "examples/Arg.java"
    , "examples/LinearSearch.java"
    , "examples/Arithmetic.java"
    , "examples/LinkedList.java"
    , "examples/BubbleSort.java"
    , "examples/Min.java"
    , "examples/Factorial.java"
    , "examples/Comparisons.java"
    , "examples/QuickSort.java"
    , "examples/Add.java"
    ]
