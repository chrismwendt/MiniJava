module Interface where

import qualified Parser as P
import qualified TypeChecker as TC
import qualified SSACompiler as SSA
import qualified RegisterAllocator as Reg
import qualified CodeGenerator as Code

registerLimit :: Int
registerLimit = 22

atParse = P.parseString
atType = TC.typeCheck . atParse
atSSA = SSA.compile . atType
atReg = Reg.allocate registerLimit . atSSA
atCode input = Code.generate (atType input) (atReg input)
