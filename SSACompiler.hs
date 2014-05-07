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

data CState = CState
    { _stProgram :: T.Program
    , _stVarToID :: M.Map String S.ID
    , _stIDToS :: M.Map S.ID S.Statement
    , _stNextID :: S.ID
    , _stNextLabel :: Int
    }

makeLenses ''CState

compile :: T.Program -> (S.Program, M.Map S.ID S.Statement)
compile program = _2 %~ _stIDToS $ runState cProgram (CState program M.empty M.empty 0 0)

cProgram :: State CState S.Program
cProgram = do
    p <- (^. stProgram) <$> get
    S.Program <$> cClass (p ^. T.pMain) <*> mapM cClass (p ^. T.pClasses)

cClass :: T.Class -> State CState S.Class
cClass (T.Class name extends vs ms) = S.Class name (map (^. AST.vName) vs) <$> mapM cMethod ms

cMethod :: T.Method -> State CState S.Method
cMethod (T.Method t name ps vs ss ret) = do
    modify $ stVarToID .~ M.empty
    (_, w) <- runWriterT $ do
        zipWithM_ cPar ps [0 .. ]
        mapM_ cVar vs
        mapM_ cSt ss
        build =<< S.Return <$> cExp ret
    return $ S.Method name w

cPar :: AST.Variable -> S.Position -> WriterT [S.ID] (State CState) S.ID
cPar (AST.Variable _ name) i = do
    p <- build (S.Parameter i)
    lift $ bind name p
    return p

cVar :: AST.Variable -> WriterT [S.ID] (State CState) S.ID
cVar (AST.Variable t name) = do
    n <- build (S.Null t)
    lift $ bind name n
    return n

cSt :: T.Statement -> WriterT [S.ID] (State CState) ()
cSt (T.Block ss) = void (mapM cSt ss)
cSt axe@(T.If cond branchTrue branchFalse) = do
    cond' <- cExp cond
    labelElse <- show <$> lift nextLabel
    labelDone <- show <$> lift nextLabel
    build (S.NBranch cond' labelElse)
    preBranchBindings <- (^. stVarToID) <$> get
    cSt branchTrue
    build (S.Goto labelDone)
    build (S.Label labelElse)
    postTrueBindings <- (^. stVarToID) <$> get
    modify $ stVarToID .~ preBranchBindings
    fromMaybe (return ()) (cSt <$> branchFalse)
    postFalseBindings <- (^. stVarToID) <$> get
    build (S.Label labelDone)
    unify postTrueBindings postFalseBindings
cSt (T.While cond body) = do
    labelStart <- show <$> lift nextLabel
    labelEnd <- show <$> lift nextLabel
    build (S.Label labelStart)
    preBranchBindings <- (^. stVarToID) <$> get
    cond' <- cExp cond
    build (S.NBranch cond' labelEnd)
    cSt body
    build (S.Goto labelStart)
    build (S.Label labelEnd)
    postBranchBindings <- (^. stVarToID) <$> get
    unify preBranchBindings postBranchBindings
cSt (T.Print e) = do
    value <- cExp e
    void $ build (S.Print value)
cSt (T.ExpressionStatement e) = void $ (: []) <$> cExp e

cExp :: T.Expression -> WriterT [S.ID] (State CState) S.ID
cExp (T.LiteralInt v) = build (S.SInt v)
cExp (T.LiteralBoolean v) = build (S.SBoolean v)
cExp (T.MemberAssignment cName object fName value) = do
    object' <- cExp object
    value' <- cExp value
    build (S.MemberAssg cName object' fName value')
cExp (T.VariableAssignment name value) = do
    bs <- (^. stVarToID) <$> get
    case M.lookup name bs of
        Just s -> do
            -- TODO consider removing VarAssg and bind name to the RHS
            v <- build =<< S.VarAssg <$> cExp value
            lift $ bind name v
            return v
        Nothing -> error "Varible not found"
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
    bs <- (^. stVarToID) <$> get
    case M.lookup name bs of
        Just s -> return s
        Nothing -> error "Varible not found"
cExp (T.NewIntArray size) = build =<< S.NewIntArray <$> cExp size
cExp (T.NewObject name) = build (S.NewObj name)
cExp (T.This) = build S.This

unify :: M.Map String S.ID -> M.Map String S.ID -> WriterT [S.ID] (State CState) ()
unify bs1 bs2 = do
    let bindings = M.assocs $ M.intersectionWith (,) bs1 bs2
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (build . uncurry S.Unify . snd) mismatches
    void $ lift $ zipWithM_ bind (map fst mismatches) unifies

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

nextID :: State CState S.ID
nextID = state $ \s -> (s ^. stNextID, stNextID %~ succ $ s)

nextLabel :: State CState Int
nextLabel = state $ \s -> (s ^. stNextLabel, stNextLabel %~ succ $ s)

build :: S.Statement -> WriterT [S.ID] (State CState) S.ID
build op = do
    id <- lift nextID
    tell [id]
    lift $ modify $ stIDToS %~ M.insert id op
    return id

bind :: String -> S.ID -> State CState ()
bind name id = modify $ stVarToID %~ M.insert name id
