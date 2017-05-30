import Criterion.Main
import Interface
import Control.Monad.Except

main = do
  source <- readFile "examples/TreeVisitor.java"
  defaultMain
    [ bench "examples/TreeVisitor.java" $ nf (runExcept . atCode) source
    ]
