{-# LANGUAGE TemplateHaskell #-}

module SSACompiler where

import qualified ASTTyped as T
import qualified AST
import qualified SSA as S
import Data.Functor
import Control.Applicative
import Control.Monad.State
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
cClass (T.Class name _ vs ms) = S.Class name (map AST._vName vs) (map cMethod ms)

cMethod :: T.Method -> S.Method
cMethod (T.Method _ name ps vs ss ret) = evalState f initialState
    where
    initialState = (CState M.empty (G.mkGraph [(0, S.BeginMethod)] []) 0)
    f = do
        zipWithM_ cPar ps [0 .. ]
        mapM_ cVar vs
        mapM_ cSt ss
        buildStep =<< S.Return <$> cExp ret
        graph <- _stGraph <$> get
        return $ S.Method name graph

cPar :: AST.Variable -> S.Position -> State CState S.ID
cPar (AST.Variable _ name) i = buildStep (S.Parameter i) >>= bind name

cVar :: AST.Variable -> State CState S.ID
cVar (AST.Variable t name) = buildStep (S.Null t) >>= bind name

cSt :: T.Statement -> State CState ()
cSt (T.Block ss) = void (mapM cSt ss)
cSt (T.If cond branchTrue branchFalse) = do
    condID <- cExp cond

    branchID <- buildStep (S.NBranch condID)

    preBranchBindings <- _stVarToID <$> get
    cSt branchTrue
    postTrueBindings <- _stVarToID <$> get

    gotoID <- buildStep (S.Goto)
    elseID <- build (S.Label)

    modify $ stVarToID .~ preBranchBindings
    fromMaybe (return ()) (cSt <$> branchFalse)
    postFalseBindings <- _stVarToID <$> get

    doneID <- buildStep (S.Label)

    modifyGraph $ G.insEdge (branchID, elseID, S.Jump)
    modifyGraph $ G.insEdge (gotoID, doneID, S.Jump)

    unify postTrueBindings postFalseBindings
cSt (T.While cond body) = do
    startID <- buildStep (S.Label)

    pre <- _stVarToID <$> get

    condID <- cExp cond
    branchID <- buildStep (S.NBranch condID)

    cSt body

    gotoID <- buildStep (S.Goto)
    doneID <- buildStep S.Label

    post <- _stVarToID <$> get

    modifyGraph $ G.insEdge (gotoID, startID, S.Jump)
    modifyGraph $ G.insEdge (branchID, doneID, S.Jump)

    unify pre post
cSt (T.Print e) = do
    value <- cExp e
    void $ buildStep (S.Print value)
cSt (T.ExpressionStatement e) = void $ (: []) <$> cExp e

cExp :: T.Expression -> State CState S.ID
cExp (T.LiteralInt v) = buildStep (S.SInt v)
cExp (T.LiteralBoolean v) = buildStep (S.SBoolean v)
cExp (T.MemberAssignment cName object fName value) = do
    object' <- cExp object
    value' <- cExp value
    buildStep (S.MemberAssg cName object' fName value')
cExp (T.VariableAssignment name value) = do
    v <- buildStep =<< S.VarAssg <$> cExp value
    bind name v
cExp (T.IndexAssignment array index value) = do
    array' <- cExp array
    index' <- cExp index
    value' <- cExp value
    buildStep (S.IndexAssg array' index' value')
cExp (T.Binary l op r) = case lookup op binaryOps of
    Just opConstructor -> buildStep =<< opConstructor <$> cExp l <*> cExp r
    Nothing -> error $ "Op " ++ show op ++ " not found in list: " ++ show (map fst binaryOps)
cExp (T.Not e) = buildStep =<< S.Not <$> cExp e
cExp (T.IndexGet a i) = buildStep =<< S.IndexGet <$> cExp a <*> cExp i
cExp (T.Call cName object mName args) = do
    object' <- cExp object
    args' <- zipWithM (\arg i -> buildStep =<< S.Arg <$> cExp arg <*> pure i) args [0 .. ]
    buildStep (S.Call cName object' mName args')
cExp (T.MemberGet cName object fName) = buildStep =<< S.MemberGet cName <$> cExp object <*> pure fName
cExp (T.VariableGet name) = do
    bs <- _stVarToID <$> get
    case M.lookup name bs of
        Just s -> return s
        Nothing -> error "Varible not found"
cExp (T.NewIntArray size) = buildStep =<< S.NewIntArray <$> cExp size
cExp (T.NewObject name) = buildStep (S.NewObj name)
cExp (T.This) = buildStep S.This

unify :: M.Map String S.ID -> M.Map String S.ID -> State CState ()
unify bs1 bs2 = do
    let bindings = M.assocs $ M.intersectionWith (,) bs1 bs2
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStep . uncurry S.Unify . snd) mismatches
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

buildStep :: S.Statement -> State CState S.ID
buildStep s = do
    pID <- _stPrevID <$> get
    buildWithPrevs [(S.Step, pID)] s

build :: S.Statement -> State CState S.ID
build = buildWithPrevs []

buildWithPrevs :: [(S.EdgeType, S.ID)] -> S.Statement -> State CState S.ID
buildWithPrevs prevs s = do
    [sID] <- G.newNodes 1 . _stGraph <$> get
    modifyGraph $ ((prevs, sID, s, []) G.&)
    modify $ stPrevID .~ sID
    return sID

bind :: String -> S.ID -> State CState S.ID
bind name sID = modify (stVarToID %~ M.insert name sID) >> return sID

modifyGraph :: (Graph -> Graph) -> State CState ()
modifyGraph f =  modify $ stGraph %~ f
