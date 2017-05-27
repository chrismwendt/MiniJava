module Interface where

import qualified Parser as P
import qualified TypeChecker as TC
import qualified SSACompiler as SSA
import qualified SSA as SSA
import qualified SSARegisters as SSAR
import qualified RegisterAllocator as Reg
import qualified CodeGenerator as Code
import Control.Monad
import qualified AST as U
import qualified ASTTyped as T
import Control.Monad.Except

registerLimit :: Int
registerLimit = 22

atParse :: String -> Except String U.Program
atParse = P.parseString

atType :: String -> Except String T.Program
atType = atParse >=> TC.typeCheck

atSSA :: String -> Except String SSA.Program
atSSA = fmap SSA.compile . atType

atReg :: String -> Except String SSAR.Program
atReg = fmap (Reg.allocate registerLimit) . atSSA

atCode :: String -> Except String String
atCode input = Code.generate <$> (atType input) <*> (atReg input)
