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
    { _stSIDToVID :: M.Map S.ID VID
    , _stVIDToVar :: SM.SetMap VID S.ID
    }

makeLenses ''RState

allocate :: Int -> S.Program -> R.Program
allocate n = aProgram n

aProgram :: Int -> S.Program -> R.Program
aProgram n p@(S.Program m cs) = R.Program (aClass n p m) (map (aClass n p) cs)

aClass :: Int -> S.Program -> S.Class -> R.Class
aClass n program c@(S.Class name fs ms) = R.Class name fs (map (aMethod program c) ms)

aMethod :: S.Program -> S.Class -> S.Method -> R.Method
aMethod program c (S.Method name ss m) = R.Method name ss m'
    where
    m' = foldr f M.empty ss
    f s = M.insert s (withRegister (fromJust $ M.lookup s m) 0)

withRegister :: S.Statement -> Either (R.Register -> R.Statement) R.Statement
withRegister (S.Load offset)            = Left  $ R.Load offset
withRegister (S.Null t)                 = Left  $ R.Null t
withRegister (S.NewObj s1)              = Left  $ R.NewObj s1
withRegister (S.NewIntArray i1)         = Left  $ R.NewIntArray i1
withRegister (S.This)                   = Left  $ R.This
withRegister (S.SInt v)                 = Left  $ R.SInt v
withRegister (S.SBoolean v)             = Left  $ R.SBoolean v
withRegister (S.Parameter i1)           = Left  $ R.Parameter i1
withRegister (S.Call s1 i1 s2 is)       = Left  $ R.Call s1 i1 s2 is
withRegister (S.MemberGet s1 i1 s2)     = Left  $ R.MemberGet s1 i1 s2
withRegister (S.MemberAssg s1 i1 s2 i2) = Left  $ R.MemberAssg s1 i1 s2 i2
withRegister (S.VarAssg i1)             = Left  $ R.VarAssg i1
withRegister (S.IndexGet i1 i2)         = Left  $ R.IndexGet i1 i2
withRegister (S.IndexAssg i1 i2 i3)     = Left  $ R.IndexAssg i1 i2 i3
withRegister (S.Not i1)                 = Left  $ R.Not i1
withRegister (S.Lt i1 i2)               = Left  $ R.Lt i1 i2
withRegister (S.Le i1 i2)               = Left  $ R.Le i1 i2
withRegister (S.Eq i1 i2)               = Left  $ R.Eq i1 i2
withRegister (S.Ne i1 i2)               = Left  $ R.Ne i1 i2
withRegister (S.Gt i1 i2)               = Left  $ R.Gt i1 i2
withRegister (S.Ge i1 i2)               = Left  $ R.Ge i1 i2
withRegister (S.And i1 i2)              = Left  $ R.And i1 i2
withRegister (S.Or i1 i2)               = Left  $ R.Or i1 i2
withRegister (S.Plus i1 i2)             = Left  $ R.Plus i1 i2
withRegister (S.Minus i1 i2)            = Left  $ R.Minus i1 i2
withRegister (S.Mul i1 i2)              = Left  $ R.Mul i1 i2
withRegister (S.Div i1 i2)              = Left  $ R.Div i1 i2
withRegister (S.Mod i1 i2)              = Left  $ R.Mod i1 i2
withRegister (S.Store i1 offset)        = Right $ R.Store i1 offset
withRegister (S.Label label)            = Right $ R.Label label
withRegister (S.Goto label)             = Right $ R.Goto label
withRegister (S.Branch i1 label)        = Right $ R.Branch i1 label
withRegister (S.NBranch i1 label)       = Right $ R.NBranch i1 label
withRegister (S.Arg i1 p)               = Right $ R.Arg i1 p
withRegister (S.Return i1)              = Right $ R.Return i1
withRegister (S.Print i1)               = Right $ R.Print i1
withRegister (S.Unify _ _)              = error "Unify should have been eliminated"