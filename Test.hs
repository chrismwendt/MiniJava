import System.Environment
import System.Process
import Control.Monad.Loops
import System.Exit
import Control.Monad
import Data.List
import SSACompiler
import qualified ASTUntyped

-- main = allM test examples >> return ()
main = do
    args <- getArgs
    case args of
        ["type"] -> do
            (_, stdout, _) <- readProcessWithExitCode "find" ["examples"] ""
            let files = filter ("java" `isSuffixOf`) $ lines stdout
            allM testType files >> return ()
        ("type":files) -> allM testType files >> return ()
        ("reg":files) -> allM testReg files >> return ()
        ["SSA", "golden", golden, file] -> ensureGolden (readProcess "runhaskell" ["Main.hs", "--stopAt", "SSA", file] "") (readProcess "cat" [golden] "")
        -- ["type", "golden", golden, file] -> ensureGolden golden file
        ["SSA"] -> allM testSSA successExamples >> return ()
        ("SSA":files) -> allM testSSA files >> return ()

ensureGolden a b = do
    outA <- a
    outB <- b
    if outA == outB
        then return ()
        else do
            putStrLn outA
            error "crud"

testSSA file = do
    putStrLn $ "Testing " ++ file

    ecCanonical <- system $ "canonical/bin/mjcompile-ssa -s " ++ file ++ " > canonical.out"
    ecMine <- system $ "runhaskell Main.hs --stopAt SSA " ++ file ++ " > mine.out"

    comp file ecCanonical ecMine

    -- system $ "canonical/bin/mjparse-ast " ++ file ++ " | runhaskell PrettySExp.hs > canonical.out"
    -- system $ "runhaskell Main.hs --stopAt parse " ++ file ++ " | runhaskell PrettySExp.hs > mine.out"

testType file = do
    ecCanonical <- system $ "canonical/bin/mjcompile-ssa -s -t " ++ file ++ " > canonical.out"
    ecMine <- system $ "runhaskell Main.hs --stopAt type " ++ file ++ " > mine.out"
    comp file ecCanonical ecMine

testReg file = do
    ecCanonical <- system $ "canonical/bin/mjcompile-ssa -s -t -r " ++ file ++ " > canonical.out"
    ecMine <- system $ "runhaskell Main.hs --stopAt reg " ++ file ++ " > mine.out"
    compDiff file ecCanonical ecMine

compDiff file ecCanonical ecMine = case (ecCanonical, ecMine) of
        (ExitSuccess, ExitSuccess) -> do
            (_, stdout, _) <- readProcessWithExitCode "diff" ["-y", "canonical.out", "mine.out"] ""
            putStr stdout
            return True
        (ExitSuccess, ExitFailure _) -> error ("Canonical succeeded, mine failed " ++ file)
        (ExitFailure _, ExitSuccess) -> error ("Canonical failed, mine succeeded " ++ file)
        (ExitFailure _, ExitFailure _) -> putStr "Both failed" >> return True

comp file ecCanonical ecMine = case (ecCanonical, ecMine) of
        (ExitSuccess, ExitSuccess) -> readProcessWithExitCode "diff" ["-y", "canonical.out", "mine.out"] "" >> return True
        (ExitSuccess, ExitFailure _) -> error ("Canonical succeeded, mine failed " ++ file)
        (ExitFailure _, ExitSuccess) -> error ("Canonical failed, mine succeeded " ++ file)
        (ExitFailure _, ExitFailure _) -> putStr "Both failed" >> return True

diffPrint a b = do
    (_, stdout, _) <- readProcessWithExitCode "diff" ["-y", a, b] ""
    putStr stdout

successExamples =
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
