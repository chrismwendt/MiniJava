{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module SSACompiler where

import qualified ASTTyped as T
import qualified AST
import qualified SSA as S
import Text.Printf
import Data.List
import Data.Functor
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Bifunctor
import Control.Lens

data CState = CState
    { _stProgram :: T.Program
    , _stNextID :: S.ID
    , _stVarToID :: M.Map String S.ID
    , _stIDToS :: M.Map S.ID S.Statement
    , _stNextLabel :: Int
    }

makeLenses ''CState

compile :: T.Program -> (S.Program, M.Map S.ID S.Statement)
compile program = let (a, s) = runState compileProgram state in (a, _stIDToS s)
    where
    state = CState
        { _stProgram = program
        , _stNextID = 0
        , _stVarToID = M.empty
        , _stIDToS = M.empty
        , _stNextLabel = 0
        }

compileProgram :: State CState S.Program
compileProgram = do
    state <- get
    let program = state ^. stProgram
    let main = state ^. stProgram . T.pMain
    let classes = state ^. stProgram . T.pClasses
    S.Program program <$> compileClass main <*> mapM compileClass classes

compileStatement :: T.Statement -> WriterT [S.ID] (State CState) ()
compileStatement (T.Block ss) = void (mapM compileStatement ss)
compileStatement axe@(T.If cond branchTrue branchFalse) = do
    cond' <- compileExpression cond

    labelElse <- (printf "l_%d" :: Int -> String) <$> lift nextLabel
    labelDone <- (printf "l_%d" :: Int -> String) <$> lift nextLabel

    buildStatement (S.NBranch cond' labelElse)

    preBranchBindings <- (^. stVarToID) <$> get

    compileStatement branchTrue
    buildStatement (S.Goto labelDone)
    buildStatement (S.Label labelElse)
    postTrueBindings <- (^. stVarToID) <$> get

    modify $ stVarToID .~ preBranchBindings
    case branchFalse of
        Just b -> compileStatement b
        Nothing -> return ()
    postFalseBindings <- (^. stVarToID) <$> get

    buildStatement (S.Label labelDone)

    let bindings = M.assocs $ M.intersectionWith (,) postTrueBindings postFalseBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStatement . uncurry S.Unify . snd) mismatches
    lift $ zipWithM_ insertVarToID (map fst mismatches) unifies

    return ()
compileStatement (T.While cond body) = do
    labelStart <- (printf "l_%d" :: Int -> String) <$> lift nextLabel
    labelEnd <- (printf "l_%d" :: Int -> String) <$> lift nextLabel

    buildStatement (S.Label labelStart)

    preBranchState <- get
    let preBranchBindings = preBranchState ^. stVarToID

    cond' <- compileExpression cond

    buildStatement (S.NBranch cond' labelEnd)

    compileStatement body

    buildStatement (S.Goto labelStart)
    buildStatement (S.Label labelEnd)

    postBranchBindings <- (^. stVarToID) <$> get

    let bindings = M.assocs $ M.intersectionWith (,) preBranchBindings postBranchBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStatement . uncurry S.Unify . snd) mismatches
    lift $ zipWithM_ insertVarToID (map fst mismatches) unifies

    return ()
compileStatement (T.Print e) = do
    value <- compileExpression e
    buildStatement (S.Print value)
    return ()
compileStatement (T.ExpressionStatement e) = (: []) <$> compileExpression e >> return ()

compileExpression :: T.Expression -> WriterT [S.ID] (State CState) S.ID
compileExpression (T.LiteralInt v) = buildStatement (S.SInt v)
compileExpression (T.LiteralBoolean v) = buildStatement (S.SBoolean v)
compileExpression (T.MemberAssignment cName object fName value) = do
    object' <- compileExpression object
    value' <- compileExpression value
    buildStatement (S.MemberAssg cName object' fName value')
compileExpression (T.VariableAssignment name value) = do
    bs <- _stVarToID <$> get
    case M.lookup name bs of
        Just s -> do
            value' <- compileExpression value
            v <- buildStatement (S.VarAssg value')
            lift $ insertVarToID name value'
            return v
        Nothing -> error "Varible not found"
compileExpression (T.IndexAssignment array index value) = do
    array' <- compileExpression array
    index' <- compileExpression index
    value' <- compileExpression value
    buildStatement (S.IndexAssg array' index' value')
compileExpression (T.Binary l op r) = do
    sl <- compileExpression l
    sr <- compileExpression r
    case lookup op binaryOps of
        Just opConstructor -> buildStatement (opConstructor sl sr)
        Nothing -> error $ "Op " ++ show op ++ " not found in list: " ++ show (map fst binaryOps)
compileExpression (T.Not e) = do
    r <- compileExpression e
    buildStatement (S.Not r)
compileExpression (T.IndexGet array index) = do
    array <- compileExpression array
    index <- compileExpression index
    buildStatement (S.IndexGet array index)
compileExpression (T.Call cName object mName args) = do
    object' <- compileExpression object
    let makeArg arg i = do
        target <- compileExpression arg
        buildStatement (S.Arg target i)
    args' <- zipWithM makeArg args [0 .. ]
    buildStatement (S.Call cName object' mName args')
compileExpression (T.MemberGet cName object fName) = do
    object' <- compileExpression object
    buildStatement (S.MemberGet cName object' fName)
compileExpression (T.VariableGet name) = do
    bs <- _stVarToID <$> get
    case M.lookup name bs of
        Just s -> return s
        Nothing -> error "Varible not found"
compileExpression (T.NewIntArray size) = do
    array <- compileExpression size
    buildStatement (S.NewIntArray array)
compileExpression (T.NewObject name) = buildStatement (S.NewObj name)
compileExpression (T.This) = buildStatement S.This

compileClass :: T.Class -> State CState S.Class
compileClass ast@(T.Class name extends vs ms) =
    S.Class ast <$> zipWithM compileVariableAsField vs [0 .. ] <*> mapM compileMethod ms

compileVariableAsField :: AST.Variable -> S.Position -> State CState S.Field
compileVariableAsField v i = return $ S.Field v i

compileMethod :: T.Method -> State CState S.Method
compileMethod ast@(T.Method t name ps vs ss ret) = do
    modify $ stVarToID .~ M.empty
    (a, w) <- runWriterT $ do
        ssaParams <- zipWithM (curry $ buildStatement . S.Parameter . snd) ps [0 .. ]
        ssaVarAssgs <- mapM (buildStatement . S.VarAssg) ssaParams
        lift $ zipWithM insertVarToID (map (^. AST.vName) ps) ssaVarAssgs
        mapM compileVariable vs
        mapM compileStatement ss
        ret' <- compileExpression ret
        buildStatement (S.Return ret')
        return ()
    return $ S.Method ast w

compileVariable :: AST.Variable -> WriterT [S.ID] (State CState) S.ID
compileVariable (AST.Variable t name) = do
    r <- buildStatement (S.Null t)
    modify $ stVarToID %~ M.insert name r
    return r

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

buildStatement :: S.Statement -> WriterT [S.ID] (State CState) S.ID
buildStatement op = do
    id <- lift nextID
    tell [id]
    lift $ modify $ stIDToS %~ M.insert id op
    return id

insertVarToID :: String -> S.ID -> State CState ()
insertVarToID name id = modify $ stVarToID %~ M.insert name id
