module TypeCheckerAST where

import qualified ASTUntyped as U
import qualified ASTTyped as T
import qualified Data.Map as M
import Safe
import Data.Maybe
import Data.List

typeCheck :: U.Program -> T.Program
typeCheck (U.Program { U._pMain = main, U._pClasses = classes }) = T.Program (T.Print (T.LiteralInt 1337)) []
