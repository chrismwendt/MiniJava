{-# LANGUAGE TemplateHaskell #-}

module RegisterAllocator where

import qualified ASTTyped as T
import qualified AST
import qualified SSA as S
import qualified SSARegisters as R
import Data.Functor
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.SetMap as SM
import Data.Maybe
import Control.Lens

type VID = Int

data RState = RState
    { _stVIDToVar :: SM.SetMap VID S.ID
    }

makeLenses ''RState

allocate :: Int -> S.Program -> R.Program
allocate n = aProgram n

aProgram :: Int -> S.Program -> R.Program
aProgram n (S.Program m cs) = R.Program (aClass n m) (map (aClass n) cs)

aClass :: Int -> S.Program -> S.Class -> R.Class
aClass n program c@(S.Class name fs ms) = R.Class name fs (map (aMethod program c) ms)

aMethod :: S.Program -> S.Class -> S.Method -> R.Method
aMethod program c (S.Method name ss m) = R.Method
