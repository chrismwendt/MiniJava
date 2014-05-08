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

withRegister :: S.Statement -> R.Register -> R.Statement
withRegister s r = case s of
    S.Load offset            -> R.Load offset r
    S.Null t                 -> R.Null t r
    S.NewObj s1              -> R.NewObj s1 r
    S.NewIntArray i1         -> R.NewIntArray i1 r
    S.This                   -> R.This r
    S.SInt v                 -> R.SInt v r
    S.SBoolean v             -> R.SBoolean v r
    S.Parameter i1           -> R.Parameter i1 r
    S.Call s1 i1 s2 is       -> R.Call s1 i1 s2 is r
    S.MemberGet s1 i1 s2     -> R.MemberGet s1 i1 s2 r
    S.MemberAssg s1 i1 s2 i2 -> R.MemberAssg s1 i1 s2 i2 r
    S.VarAssg i1             -> R.VarAssg i1 r
    S.IndexGet i1 i2         -> R.IndexGet i1 i2 r
    S.IndexAssg i1 i2 i3     -> R.IndexAssg i1 i2 i3 r
    S.Not i1                 -> R.Not i1 r
    S.Lt i1 i2               -> R.Lt i1 i2 r
    S.Le i1 i2               -> R.Le i1 i2 r
    S.Eq i1 i2               -> R.Eq i1 i2 r
    S.Ne i1 i2               -> R.Ne i1 i2 r
    S.Gt i1 i2               -> R.Gt i1 i2 r
    S.Ge i1 i2               -> R.Ge i1 i2 r
    S.And i1 i2              -> R.And i1 i2 r
    S.Or i1 i2               -> R.Or i1 i2 r
    S.Plus i1 i2             -> R.Plus i1 i2 r
    S.Minus i1 i2            -> R.Minus i1 i2 r
    S.Mul i1 i2              -> R.Mul i1 i2 r
    S.Div i1 i2              -> R.Div i1 i2 r
    S.Mod i1 i2              -> R.Mod i1 i2 r
    _                        -> error $ show s ++ " cannot have a register"

withoutRegister :: S.Statement -> R.Register -> R.Statement
withoutRegister (Unify i1 i2) = R.Unify i1 i2
withoutRegister (Store i1 offset) = Store i1 offset
withoutRegister (Label label) = Label label
withoutRegister (Goto label) = Goto label
withoutRegister (Branch i1 label) = Branch i1 label
withoutRegister (NBranch i1 label) = NBranch i1 label
withoutRegister (Arg i1 Position) = Arg i1 Position
withoutRegister (Return i1) = Return i1
withoutRegister (Print i1) = Print i1
withoutRegister s = error $ show s ++ " needs a register"
