{-# LANGUAGE TemplateHaskell #-}

module RegisterAllocator where

import qualified AST as AST
import SSACompiler
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.List
import Control.Lens

type Register = Int

type Variable = S.Set (SSAStatement ID StaticType)

data CFNode = CFNode
    { _defs  :: S.Set Variable
    , _uses  :: S.Set Variable
    , _preds :: S.Set Variable
    , _succs :: S.Set Variable
    , _ins   :: S.Set Variable
    , _outs  :: S.Set Variable
    }
    deriving (Show)

data TempNode = TempNode
    { _color :: Register
    , _variable :: Variable
    , _neighbors :: S.Set TempNode
    }

data RegState = RegState
    { _registerCount :: Int
    , _sToV :: M.Map ID Variable
    , _vToT :: M.Map Variable TempNode
    , _nonSpills :: S.Set TempNode
    , _potentialSpills :: S.Set TempNode
    , _vToSSAs :: M.Map Variable (S.Set ID)
    , _spillCount :: Int
    , _idList :: [ID]
    , _idToS :: M.Map ID (SSAStatement ID StaticType)
    , _idToS' :: M.Map ID (SSAStatement ID (StaticType, Register))
    }

makeLenses ''CFNode
makeLenses ''TempNode
makeLenses ''RegState

allocate ::
       Int
    -> SSAProgram ID StaticType
    -> [ID]
    -> M.Map ID (SSAStatement ID StaticType)
    -> (SSAProgram ID (StaticType, Register), [ID], M.Map ID (SSAStatement ID (StaticType, Register)))
allocate registerCount program ss m = (a, s ^. idList, s ^. idToS')
    where
    (a, s) = runState (allocProgram program) RegState
        { _registerCount = registerCount
        , _sToV = M.empty
        , _vToT = M.empty
        , _nonSpills = S.empty
        , _potentialSpills = S.empty
        , _vToSSAs = M.empty
        , _spillCount = 0
        , _idList = ss
        , _idToS = m
        , _idToS' = M.empty
        }

allocProgram :: SSAProgram ID StaticType -> State RegState (SSAProgram ID (StaticType, Register))
allocProgram program = undefined
