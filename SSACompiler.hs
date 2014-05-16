{-# LANGUAGE TemplateHaskell #-}

module SSACompiler where

import Data.Functor
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Control.Lens
import qualified Data.Graph.Inductive as G
import qualified AST as U
import qualified ASTTyped as T
import qualified SSA as SSA

type ControlFlowGraph = G.Gr SSA.Statement SSA.EdgeType

data CState = CState
    { _stVarToID :: M.Map String SSA.ID
    , _stGraph :: ControlFlowGraph
    , _stPrevID :: SSA.ID
    }

makeLenses ''CState

compile :: T.Program -> SSA.Program
compile = compileProgram

compileProgram :: T.Program -> SSA.Program
compileProgram (T.Program m cs) = SSA.Program (cClass m) (map cClass cs)

cClass :: T.Class -> SSA.Class
cClass (T.Class name _ vs ms) = SSA.Class name (map U._vName vs) (map cMethod ms)

cMethod :: T.Method -> SSA.Method
cMethod (T.Method _ name ps vs ss ret) = evalState cMethod' initialState
    where
    initialState = (CState M.empty (G.mkGraph [(0, SSA.BeginMethod)] []) 0)
    cMethod' = do
        zipWithM_ cParameter ps [0 .. ]
        mapM_ cVariable vs
        mapM_ cSt ss
        buildStep =<< SSA.Return <$> cExp ret
        graph <- gets _stGraph
        return $ SSA.Method name graph

cParameter :: U.Variable -> SSA.Position -> State CState SSA.ID
cParameter (U.Variable _ name) i = buildStep (SSA.Parameter i) >>= bind name

cVariable :: U.Variable -> State CState SSA.ID
cVariable (U.Variable t name) = buildStep (SSA.Null t) >>= bind name

cSt :: T.Statement -> State CState ()
cSt (T.Block ss) = mapM_ cSt ss
cSt (T.If cond branchTrue branchFalse) = do
    cond' <- cExp cond

    pre <- gets _stVarToID

    branch <- buildStep (SSA.NBranch cond')
    cSt branchTrue
    trueGotoDone <- buildStep (SSA.Goto)

    postTrue <- gets _stVarToID
    modify $ stVarToID .~ pre

    elseLabel <- build (SSA.Label)
    fromMaybe (return ()) (cSt <$> branchFalse)
    falseGotoDone <- buildStep (SSA.Goto)

    postFalse <- gets _stVarToID

    doneLabel <- buildStep (SSA.Label)

    modifyGraph $ G.insEdge (branch, elseLabel, SSA.Jump)
    modifyGraph $ G.insEdge (trueGotoDone, doneLabel, SSA.Jump)
    modifyGraph $ G.insEdge (falseGotoDone, doneLabel, SSA.Jump)

    unify postTrue postFalse
cSt (T.While cond body) = do
    pre <- gets _stVarToID

    conditionLabel <- buildStep (SSA.Label)
    cond' <- cExp cond
    branch <- buildStep (SSA.NBranch cond')

    cSt body
    goto <- buildStep (SSA.Goto)

    post <- gets _stVarToID

    doneLabel <- build SSA.Label

    modifyGraph $ G.insEdge (goto, conditionLabel, SSA.Jump)
    modifyGraph $ G.insEdge (branch, doneLabel, SSA.Jump)

    unify pre post
cSt (T.Print e) = do
    value <- cExp e
    void $ buildStep (SSA.Print value)
cSt (T.ExpressionStatement e) = void $ (: []) <$> cExp e

cExp :: T.Expression -> State CState SSA.ID
cExp (T.LiteralInt v) = buildStep (SSA.SInt v)
cExp (T.LiteralBoolean v) = buildStep (SSA.SBoolean v)
cExp (T.MemberAssignment cName object fName value) = do
    object' <- cExp object
    value' <- cExp value
    buildStep (SSA.MemberAssg cName object' fName value')
cExp (T.VariableAssignment name value) = do
    v <- buildStep =<< SSA.VarAssg <$> cExp value
    bind name v
cExp (T.IndexAssignment array index value) = do
    array' <- cExp array
    index' <- cExp index
    value' <- cExp value
    buildStep (SSA.IndexAssg array' index' value')
cExp (T.Binary l op r) = case lookup op binaryOps of
    Just opConstructor -> buildStep =<< opConstructor <$> cExp l <*> cExp r
    Nothing -> error $ "Op " ++ show op ++ " not found in list: " ++ show (map fst binaryOps)
cExp (T.Not e) = buildStep =<< SSA.Not <$> cExp e
cExp (T.IndexGet a i) = buildStep =<< SSA.IndexGet <$> cExp a <*> cExp i
cExp (T.Call cName object mName args) = do
    object' <- cExp object
    args' <- zipWithM (\arg i -> buildStep =<< SSA.Arg <$> cExp arg <*> pure i) args [0 .. ]
    buildStep (SSA.Call cName object' mName args')
cExp (T.MemberGet cName object fName) = do
    object' <- cExp object
    buildStep $ SSA.MemberGet cName object' fName
cExp (T.VariableGet name) = do
    bs <- gets _stVarToID
    case M.lookup name bs of
        Just s -> return s
        Nothing -> error "Varible not found"
cExp (T.NewIntArray size) = buildStep =<< SSA.NewIntArray <$> cExp size
cExp (T.NewObject name) = buildStep (SSA.NewObj name)
cExp (T.IntArrayLength array) = buildStep =<< SSA.ArrayLength <$> cExp array
cExp (T.This) = buildStep SSA.This

unify :: M.Map String SSA.ID -> M.Map String SSA.ID -> State CState ()
unify bs1 bs2 = do
    let bindings = M.assocs $ M.intersectionWith (,) bs1 bs2
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStep . uncurry SSA.Unify . snd) mismatches
    zipWithM_ bind (map fst mismatches) unifies

binaryOps :: [(U.BinaryOperator, SSA.ID -> SSA.ID -> SSA.Statement)]
binaryOps =
    [ (U.Lt, SSA.Lt)
    , (U.Le, SSA.Le)
    , (U.Eq, SSA.Eq)
    , (U.Ne, SSA.Ne)
    , (U.Gt, SSA.Gt)
    , (U.Ge, SSA.Ge)
    , (U.And, SSA.And)
    , (U.Or, SSA.Or)
    , (U.Plus, SSA.Plus)
    , (U.Minus, SSA.Minus)
    , (U.Mul, SSA.Mul)
    , (U.Div, SSA.Div)
    , (U.Mod, SSA.Mod)
    ]

buildStep :: SSA.Statement -> State CState SSA.ID
buildStep s = do
    pID <- gets _stPrevID
    buildWithPrevs [(SSA.Step, pID)] s

build :: SSA.Statement -> State CState SSA.ID
build = buildWithPrevs []

buildWithPrevs :: [(SSA.EdgeType, SSA.ID)] -> SSA.Statement -> State CState SSA.ID
buildWithPrevs prevs s = do
    [sID] <- G.newNodes 1 <$> gets _stGraph
    modifyGraph $ ((prevs, sID, s, []) G.&)
    modify $ stPrevID .~ sID
    return sID

bind :: String -> SSA.ID -> State CState SSA.ID
bind name sID = modify (stVarToID %~ M.insert name sID) >> return sID

modifyGraph :: (ControlFlowGraph -> ControlFlowGraph) -> State CState ()
modifyGraph f = modify $ stGraph %~ f
