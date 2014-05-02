{-# LANGUAGE TemplateHaskell #-}

module RegisterAllocator where

import qualified AST as AST
import SSACompiler
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.List
import Control.Lens
import Data.Functor

type Register = Int

type Variable = S.Set (SSAStatement StaticType ID)

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
    , _idToS :: M.Map ID (SSAStatement StaticType ID)
    , _idToS' :: M.Map ID (SSAStatement (StaticType, Register) ID)
    }

makeLenses ''CFNode
makeLenses ''TempNode
makeLenses ''RegState

allocate ::
       Int
    -> SSAProgram StaticType ID
    -> [ID]
    -> M.Map ID (SSAStatement StaticType ID)
    -> (SSAProgram (StaticType, Register) ID, [ID], M.Map ID (SSAStatement (StaticType, Register) ID))
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

allocProgram :: SSAProgram StaticType ID -> State RegState (SSAProgram (StaticType, Register) ID)
allocProgram (SSAProgram ast@(AST.Program s cs) ids classes) = do
    let mainMethod = SSAMethod (AST.MethodDecl (AST.ObjectType "") "" [] [] [s] AST.ThisExp) [] [] 0
    mainIDs <- (^. mStatements) <$> allocMethod mainMethod
    classes' <- mapM allocClass classes
    return $ SSAProgram ast mainIDs classes'

allocClass :: SSAClass StaticType ID -> State RegState (SSAClass (StaticType, Register) ID)
allocClass (SSAClass classDecl@(AST.ClassDecl c _ _ _) fields methods) = do
    fields' <- mapM allocField fields
    methods' <- mapM allocMethod methods
    return $ SSAClass classDecl fields' methods'

allocField :: SSAField StaticType -> State RegState (SSAField (StaticType, Register))
allocField (SSAField ast index id) = return $ SSAField ast index (id, 0)

-- XXX ignore the return value in the SSAMethod
allocMethod :: SSAMethod StaticType ID -> State RegState (SSAMethod (StaticType, Register) ID)
allocMethod = undefined
-- allocMethod (SSAMethod methodDecl varIDs ids retID) = do
--     return $ SSAMethod methodDecl varIDs ids' retID
