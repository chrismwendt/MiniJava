{-# LANGUAGE TemplateHaskell #-}

module SSACompiler where

import qualified ASTTyped as T
import qualified AST
import qualified SSA as S
import Data.Functor
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Maybe
import Control.Lens
import qualified Data.Graph.Inductive as G

type Graph = G.Gr S.Statement S.EdgeType

data CState = CState
    { _stVarToID :: M.Map String S.ID
    , _stGraph :: Graph
    , _stPrevID :: S.ID
    }

makeLenses ''CState

compile :: T.Program -> S.Program
compile (T.Program m cs) = S.Program (cClass m) (map cClass cs)

cClass :: T.Class -> S.Class
cClass (T.Class name extends vs ms) = S.Class name (map AST._vName vs) (map cMethod ms)

cMethod :: T.Method -> S.Method
cMethod (T.Method _ name ps vs ss ret) = evalState f initialState
    where
    initialState = (CState M.empty (G.mkGraph [(0, S.BeginMethod)] []) 0)
    f = do
        zipWithM_ cPar ps [0 .. ]
        mapM_ cVar vs
        mapM_ cSt ss
        build =<< S.Return <$> cExp ret
        graph <- _stGraph <$> get
        return $ S.Method name graph

cPar :: AST.Variable -> S.Position -> State CState S.ID
cPar (AST.Variable _ name) i = build (S.Parameter i) >>= bind name

cVar :: AST.Variable -> State CState S.ID
cVar (AST.Variable t name) = build (S.Null t) >>= bind name

cSt :: T.Statement -> State CState ()
cSt (T.Block ss) = void (mapM cSt ss)
cSt (T.If cond branchTrue branchFalse) = do
    cond' <- cExp cond

    [elseID, doneID] <- G.newNodes 2 . _stGraph <$> get
    modifyGraph $ G.insNodes [(elseID, S.Label), (doneID, S.Label)]

    buildSucc (S.NBranch cond') (S.Jump, elseID)

    preBranchBindings <- _stVarToID <$> get
    cSt branchTrue
    buildSucc (S.Goto) (S.Jump, doneID)
    modify $ stPrevID .~ elseID
    postTrueBindings <- _stVarToID <$> get

    modify $ stVarToID .~ preBranchBindings
    fromMaybe (return ()) (cSt <$> branchFalse)
    postFalseBindings <- _stVarToID <$> get

    pID <- _stPrevID <$> get
    modifyGraph $ G.insEdge (pID, doneID, S.Step)
    modify $ stPrevID .~ doneID

    unify postTrueBindings postFalseBindings
cSt (T.While cond body) = do
    [endID] <- G.newNodes 1 . _stGraph <$> get
    modifyGraph $ G.insNode (endID, S.Label)

    startID <- build (S.Label)
    preBranchBindings <- _stVarToID <$> get
    cond' <- cExp cond
    buildSucc (S.NBranch cond') (S.Jump, endID)
    cSt body
    buildSucc (S.Goto) (S.Jump, startID)
    pID <- _stPrevID <$> get
    modifyGraph $ G.insEdge (pID, endID, S.Step)
    modify $ stPrevID .~ endID
    postBranchBindings <- _stVarToID <$> get

    unify preBranchBindings postBranchBindings
cSt (T.Print e) = do
    value <- cExp e
    void $ build (S.Print value)
cSt (T.ExpressionStatement e) = void $ (: []) <$> cExp e

cExp :: T.Expression -> State CState S.ID
cExp (T.LiteralInt v) = build (S.SInt v)
cExp (T.LiteralBoolean v) = build (S.SBoolean v)
cExp (T.MemberAssignment cName object fName value) = do
    object' <- cExp object
    value' <- cExp value
    build (S.MemberAssg cName object' fName value')
cExp (T.VariableAssignment name value) = do
    v <- build =<< S.VarAssg <$> cExp value
    bind name v
cExp (T.IndexAssignment array index value) = do
    array' <- cExp array
    index' <- cExp index
    value' <- cExp value
    build (S.IndexAssg array' index' value')
cExp (T.Binary l op r) = case lookup op binaryOps of
    Just opConstructor -> build =<< opConstructor <$> cExp l <*> cExp r
    Nothing -> error $ "Op " ++ show op ++ " not found in list: " ++ show (map fst binaryOps)
cExp (T.Not e) = build =<< S.Not <$> cExp e
cExp (T.IndexGet a i) = build =<< S.IndexGet <$> cExp a <*> cExp i
cExp (T.Call cName object mName args) = do
    object' <- cExp object
    args' <- zipWithM (\arg i -> build =<< S.Arg <$> cExp arg <*> pure i) args [0 .. ]
    build (S.Call cName object' mName args')
cExp (T.MemberGet cName object fName) = build =<< S.MemberGet cName <$> cExp object <*> pure fName
cExp (T.VariableGet name) = do
    bs <- _stVarToID <$> get
    case M.lookup name bs of
        Just s -> return s
        Nothing -> error "Varible not found"
cExp (T.NewIntArray size) = build =<< S.NewIntArray <$> cExp size
cExp (T.NewObject name) = build (S.NewObj name)
cExp (T.This) = build S.This

unify :: M.Map String S.ID -> M.Map String S.ID -> State CState ()
unify bs1 bs2 = do
    let bindings = M.assocs $ M.intersectionWith (,) bs1 bs2
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (build . uncurry S.Unify . snd) mismatches
    void $ zipWithM_ bind (map fst mismatches) unifies

binaryOps :: [(AST.BinaryOperator, S.ID -> S.ID -> S.Statement)]
binaryOps =
    [ (AST.Lt, S.Lt)
    , (AST.Le, S.Le)
    , (AST.Eq, S.Eq)
    , (AST.Ne, S.Ne)
    , (AST.Gt, S.Gt)
    , (AST.Ge, S.Ge)
    , (AST.And, S.And)
    , (AST.Or, S.Or)
    , (AST.Plus, S.Plus)
    , (AST.Minus, S.Minus)
    , (AST.Mul, S.Mul)
    , (AST.Div, S.Div)
    , (AST.Mod, S.Mod)
    ]

buildSucc :: S.Statement -> (S.EdgeType, S.ID) -> State CState S.ID
buildSucc s su = do
    sID <- head . G.newNodes 1 . _stGraph <$> get
    pID <- _stPrevID <$> get
    modifyGraph $ (([(S.Step, pID)], sID, s, [su]) G.&)
    modify $ stPrevID .~ sID
    return sID

build :: S.Statement -> State CState S.ID
build s = do
    sID <- head . G.newNodes 1 . _stGraph <$> get
    pID <- _stPrevID <$> get
    modifyGraph $ (([(S.Step, pID)], sID, s, []) G.&)
    modify $ stPrevID .~ sID
    return sID

bind :: String -> S.ID -> State CState S.ID
bind name id = modify (stVarToID %~ M.insert name id) >> return id

modifyGraph :: (Graph -> Graph) -> State CState ()
modifyGraph f =  modify $ stGraph %~ f
