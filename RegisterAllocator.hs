module RegisterAllocator where

import qualified AST as AST
import SSACompiler
import qualified Data.Map as M
import Control.Monad.State
import Data.List

type Register = Int

allocate :: SSAProgram ID StaticType -> [ID] -> M.Map ID (SSAStatement ID StaticType) -> (SSAProgram ID (StaticType, Register), [ID], M.Map ID (SSAStatement ID (StaticType, Register)))
allocate program m = undefined
