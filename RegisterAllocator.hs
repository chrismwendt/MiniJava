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
import Data.Functor.Foldable
import Data.Functor.Compose
import Control.Monad.Fix
import Data.STRef
import Control.Monad.ST
import Control.Applicative
import Debug.Trace

type Register = Int

type Variable s = S.Set (SSAStatement StaticType (Ref s))

type Ref s = Fix (Compose (STRef s) (SSAStatement StaticType))

-- data CFNode = CFNode
--     { _defs  :: S.Set Variable
--     , _uses  :: S.Set Variable
--     , _preds :: S.Set Variable
--     , _succs :: S.Set Variable
--     , _ins   :: S.Set Variable
--     , _outs  :: S.Set Variable
--     }
--     deriving (Show)
--
-- data TempNode = TempNode
--     { _color :: Register
--     , _variable :: Variable
--     , _neighbors :: S.Set TempNode
--     }

data RegState s = RegState
    { _registerCount :: Int
    , _sToV :: M.Map (Ref s) (Variable s)
    -- , _vToT :: M.Map Variable TempNode
    -- , _nonSpills :: S.Set TempNode
    -- , _potentialSpills :: S.Set TempNode
    -- , _vToSSAs :: M.Map Variable (S.Set ID)
    , _spillCount :: Int
    }

data ThawState s = ThawState
    { _tList :: [ID]
    , _tMap :: M.Map ID (SSAStatement StaticType ID)
    , _tMapRef :: M.Map ID (Ref s)
    }

-- makeLenses ''CFNode
-- makeLenses ''TempNode
makeLenses ''RegState
makeLenses ''ThawState

thaw :: SSAProgram StaticType ID -> [ID] -> M.Map ID (SSAStatement StaticType ID) -> ST s (SSAProgram StaticType (Ref s))
thaw program ssaList ssaMap = evalState (thawProgram program) ThawState
    { _tList = ssaList
    , _tMap = M.empty
    , _tMapRef = M.empty
    }

thawProgram :: SSAProgram StaticType ID -> State (ThawState s) (ST s (SSAProgram StaticType (Ref s)))
thawProgram (SSAProgram ast ss cs) = do
    ss' <- mapM thawStatement ss
    cs' <- mapM thawClass cs
    return $ do
        ss'' <- sequence ss'
        cs'' <- sequence cs'
        return $ SSAProgram ast ss'' cs''

thawClass :: SSAClass StaticType ID -> State (ThawState s) (ST s (SSAClass StaticType (Ref s)))
thawClass (SSAClass classDecl fs ms) = do
    ms' <- mapM thawMethod ms
    return $ do
        ms'' <- sequence ms'
        return $ SSAClass classDecl fs ms''

thawMethod :: SSAMethod StaticType ID -> State (ThawState s) (ST s (SSAMethod StaticType (Ref s)))
thawMethod (SSAMethod methodDecl ps ss ret) = do
    ps' <- mapM thawStatement ps
    ss' <- mapM thawStatement ss
    ret' <- thawStatement ret
    return $ do
        ps'' <- sequence ps'
        ss'' <- sequence ss'
        ret'' <- ret'
        return $ SSAMethod methodDecl ps'' ss'' ret''

thawStatement :: ID -> State (ThawState s) (ST s (Ref s))
thawStatement sID = do
    m <- (^. tMap) <$> get
    mRef <- (^. tMapRef) <$> get
    return =<< case M.lookup sID m of
        Just ssa -> return $ Fix . Compose <$> (newSTRef $ bimap id (\sID' -> case M.lookup sID' mRef of
                Just x -> traceShow sID' x
                Nothing -> error $ "bimap id " ++ show sID ++ " not found") ssa)
        Nothing -> error $ "sID " ++ show sID ++ " not found"

freeze :: SSAProgram (StaticType, Register) (Ref s) -> ST s (SSAProgram (StaticType, Register) ID, [ID], M.Map ID (SSAStatement (StaticType, Register) ID))
freeze = error "freeze unimplemented"

allocate ::
       Int
    -> SSAProgram StaticType ID
    -> [ID]
    -> M.Map ID (SSAStatement StaticType ID)
    -> (SSAProgram (StaticType, Register) ID, [ID], M.Map ID (SSAStatement (StaticType, Register) ID))
allocate registerCount program ssaList ssaMap = runST $ do
    mutableProgram <- thaw program ssaList ssaMap
    mutableProgram' <- evalState (allocProgram mutableProgram) RegState
        { _registerCount = registerCount
        , _sToV = M.empty
        -- , _vToT = M.empty
        -- , _nonSpills = S.empty
        -- , _potentialSpills = S.empty
        -- , _vToSSAs = M.empty
        , _spillCount = 0
        }
    freeze mutableProgram'

allocProgram :: SSAProgram StaticType (Ref s) -> State (RegState s) (ST s (SSAProgram (StaticType, Register) (Ref s)))
allocProgram (SSAProgram ast@(AST.Program s cs) ids classes) = do
    return $ do
        return $ SSAProgram ast [] []

-- allocProgram :: SSAProgram StaticType ID -> State RegState (SSAProgram (StaticType, Register) ID)
-- allocProgram (SSAProgram ast@(AST.Program s cs) ids classes) = do
--     let mainMethod = SSAMethod (AST.MethodDecl (AST.ObjectType "") "" [] [] [s] AST.ThisExp) [] [] 0
--     mainIDs <- (^. mStatements) <$> allocMethod mainMethod
--     classes' <- mapM allocClass classes
--     return $ SSAProgram ast mainIDs classes'
--
-- allocClass :: SSAClass StaticType ID -> State RegState (SSAClass (StaticType, Register) ID)
-- allocClass (SSAClass classDecl@(AST.ClassDecl c _ _ _) fields methods) = do
--     fields' <- mapM allocField fields
--     methods' <- mapM allocMethod methods
--     return $ SSAClass classDecl fields' methods'
--
-- allocField :: SSAField StaticType -> State RegState (SSAField (StaticType, Register))
-- allocField (SSAField ast index id) = return $ SSAField ast index (id, 0)
--
-- -- XXX ignore the return value in the SSAMethod
-- allocMethod :: SSAMethod StaticType ID -> State RegState (SSAMethod (StaticType, Register) ID)
-- allocMethod = undefined
-- allocMethod (SSAMethod methodDecl varIDs ids retID) = do
--     return $ SSAMethod methodDecl varIDs ids' retID
