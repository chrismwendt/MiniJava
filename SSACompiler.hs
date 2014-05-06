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
    , _stIDList :: [S.ID]
    , _stNextLabel :: Int
    }

makeLenses ''CState

ssaCompile :: T.Program -> (S.Program, M.Map S.ID S.Statement)
ssaCompile program = let (a, s) = runState scProgram state in (a, _stIDToS s)
    where
    state = CState
        { _stProgram = program
        , _stNextID = 0
        , _stVarToID = M.empty
        , _stIDToS = M.empty
        , _stIDList = []
        , _stNextLabel = 0
        }

scProgram :: State CState S.Program
scProgram = do
    state <- get
    let program = state ^. stProgram
    let main = state ^. stProgram . T.pMain
    let classes = state ^. stProgram . T.pClasses
    S.Program program <$> (scStatement main >> (^. stIDList) <$> get) <*> mapM scClass classes

scStatement :: T.Statement -> State CState ()
scStatement (T.Block ss) = void (mapM scStatement ss)
scStatement axe@(T.If cond branchTrue branchFalse) = do
    cond' <- sc cond

    labelElse <- (printf "l_%d" :: Int -> String) <$> nextLabel
    labelDone <- (printf "l_%d" :: Int -> String) <$> nextLabel

    buildStatement (S.NBranch cond' labelElse)

    preBranchBindings <- (^. stVarToID) <$> get

    scStatement branchTrue
    buildStatement (S.Goto labelDone)
    buildStatement (S.Label labelElse)
    postTrueBindings <- (^. stVarToID) <$> get

    modify $ stVarToID .~ preBranchBindings
    case branchFalse of
        Just b -> scStatement b
        Nothing -> return ()
    postFalseBindings <- (^. stVarToID) <$> get

    buildStatement (S.Label labelDone)

    let bindings = M.assocs $ M.intersectionWith (,) postTrueBindings postFalseBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStatement . uncurry S.Unify . snd) mismatches
    zipWithM_ insertVarToID (map fst mismatches) unifies

    return ()
scStatement (T.While cond body) = do
    labelStart <- (printf "l_%d" :: Int -> String) <$> nextLabel
    labelEnd <- (printf "l_%d" :: Int -> String) <$> nextLabel

    buildStatement (S.Label labelStart)

    preBranchState <- get
    let preBranchBindings = preBranchState ^. stVarToID

    cond' <- sc cond

    buildStatement (S.NBranch cond' labelEnd)

    scStatement body

    buildStatement (S.Goto labelStart)
    buildStatement (S.Label labelEnd)

    postBranchBindings <- (^. stVarToID) <$> get

    let bindings = M.assocs $ M.intersectionWith (,) preBranchBindings postBranchBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStatement . uncurry S.Unify . snd) mismatches
    zipWithM_ insertVarToID (map fst mismatches) unifies

    return ()
scStatement (T.Print e) = do
    value <- sc e
    buildStatement (S.Print value)
    return ()
scStatement (T.ExpressionStatement e) = singleton <$> sc e >> return ()

sc :: T.Expression -> State CState S.ID
sc (T.LiteralInt v) = buildStatement (S.SInt v)
sc (T.LiteralBoolean v) = buildStatement (S.SBoolean v)
sc (T.MemberAssignment cName object fName value) = do
    object' <- sc object
    value' <- sc value
    buildStatement (S.MemberAssg cName object' fName value')
sc (T.VariableAssignment name value) = do
    bs <- _stVarToID <$> get
    case M.lookup name bs of
        Just s -> do
            value' <- sc value
            v <- buildStatement (S.VarAssg name value')
            insertVarToID name value'
            return v
        Nothing -> error "Varible not found"
sc (T.IndexAssignment array index value) = do
    array' <- sc array
    index' <- sc index
    value' <- sc value
    buildStatement (S.IndexAssg array' index' value')
sc (T.Binary l op r) = do
    sl <- sc l
    sr <- sc r
    case lookup op binaryOps of
        Just opConstructor -> buildStatement (opConstructor sl sr)
        Nothing -> error $ "Op " ++ show op ++ " not found in list: " ++ show (map fst binaryOps)
sc (T.Not e) = do
    r <- sc e
    buildStatement (S.Not r)
sc (T.IndexGet array index) = do
    array <- sc array
    index <- sc index
    buildStatement (S.IndexGet array index)
sc (T.Call cName object mName args) = do
    object' <- sc object
    let makeArg arg i = do
        target <- sc arg
        buildStatement (S.Arg target i)
    args' <- zipWithM makeArg args [0 .. ]
    buildStatement (S.Call cName object' mName args')
sc (T.MemberGet cName object fName) = do
    object' <- sc object
    buildStatement (S.MemberGet cName object' fName)
sc (T.VariableGet name) = do
    bs <- _stVarToID <$> get
    case M.lookup name bs of
        Just s -> return s
        Nothing -> error "Varible not found"
sc (T.NewIntArray size) = do
    array <- sc size
    buildStatement (S.NewIntArray array)
sc (T.NewObject name) = buildStatement (S.NewObj name)
sc (T.This) = buildStatement S.This

scClass :: T.Class -> State CState S.Class
scClass ast@(T.Class name extends vs ms) =
    S.Class ast <$> zipWithM scVariableAsField vs [0 .. ] <*> mapM scMethod ms

scVariableAsField :: AST.Variable -> S.Position -> State CState S.Field
scVariableAsField v i = return $ S.Field v i

scMethod :: T.Method -> State CState (S.Method)
scMethod ast@(T.Method t name ps vs ss ret) = do
    modify $ stIDList .~ []
    ssaParams <- mapM (buildStatement . S.Parameter) [0 .. ]
    ssaVarAssgs <- mapM buildStatement $ zipWith S.VarAssg (map (^. AST.vName) ps) ssaParams
    zipWithM insertVarToID (map (^. AST.vName) ps) ssaVarAssgs
    mapM scVariable vs
    mapM scStatement ss
    ret' <- sc ret
    buildStatement (S.Return ret')
    allStatements <- (^. stIDList) <$> get
    return $ S.Method ast ssaParams allStatements ret'

scVariable :: AST.Variable -> State CState S.ID
scVariable (AST.Variable t name) = do
    r <- buildStatement (S.Null t)
    modify $ stVarToID %~ M.insert name r
    return r

singleton :: a -> [a]
singleton a = [a]

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

buildStatement :: S.Statement -> State CState S.ID
buildStatement op = do
    id <- nextID
    modify $ stIDList %~ (++ [id])
    modify $ stIDToS %~ M.insert id op
    return id

insertVarToID :: String -> S.ID -> State CState ()
insertVarToID name id = modify $ stVarToID %~ M.insert name id
