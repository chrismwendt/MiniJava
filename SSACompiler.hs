{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module SSACompiler where

import qualified AST
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

type ID = Int

data SSAProgram info ref = SSAProgram
    { _pProgram :: AST.Program
    , _pMain :: [ref]
    , _pClasses :: [SSAClass info ref]
    }

data SSAClass info ref = SSAClass
    { _cClassDecl :: AST.ClassDecl
    , _cFields :: [SSAField info]
    , _cMethod :: [SSAMethod info ref]
    }

data SSAField info = SSAField
    { _fVarDecl :: AST.VarDecl
    , _fPosition :: Int
    , _fInfo :: info
    }
    deriving (Functor)

data SSAMethod info ref = SSAMethod
    { _mMethodDecl :: AST.MethodDecl
    , _mParameters :: [ref]
    , _mStatements :: [ref]
    , _mReturn :: ref
    }

data StaticType =
      TypeInt
    | TypeBoolean
    | TypeVoid
    | TypeObject
        { _tObject :: StaticTypeObject
        }
    deriving (Eq)

data StaticTypeObject = StaticTypeObject
    { toName :: String
    , toParent :: (Maybe StaticTypeObject)
    }
    deriving (Eq)

data SSAStatement info ref = SSAStatement
    { _sID :: ref
    , _sOp :: SSAOp ref
    , _sInfo :: info
    }
    deriving (Eq)

data SSAOp ref =
      Unify ref ref
    | Alias ref
    | This
    | Parameter AST.Parameter Int
    | Arg ref Int
    | Null AST.Type
    | SInt Int
    | SBoolean Bool
    | NewObj String
    | NewIntArray ref
    | Label String
    | Goto String
    | Branch ref String
    | NBranch ref String
    | Call String ref [ref]
    | Print ref
    | Return ref
    | Member ref String
    | Index ref ref
    | Store ref Int
    | Load Int
    | VarAssg ref String
    | MemberAssg ref ref String
    | IndexAssg ref ref ref
    | Not ref
    | Lt ref ref
    | Le ref ref
    | Eq ref ref
    | Ne ref ref
    | Gt ref ref
    | Ge ref ref
    | And ref ref
    | Or ref ref
    | Plus ref ref
    | Minus ref ref
    | Mul ref ref
    | Div ref ref
    | Mod ref ref
    deriving (Eq, Functor)

data SSAState info ref = SSAState
    { _stProgram :: AST.Program
    , _stNextID :: ID
    , _stVarToID :: M.Map String ID
    , _stIDToS :: M.Map ID (SSAStatement info ref)
    , _stIDList :: [ID]
    , _stNextLabel :: Int
    }

makeLenses ''SSAProgram
makeLenses ''SSAClass
makeLenses ''SSAField
makeLenses ''SSAMethod
makeLenses ''StaticType
makeLenses ''StaticTypeObject
makeLenses ''SSAStatement
makeLenses ''SSAState

instance Show StaticTypeObject where
    show (StaticTypeObject name _) = name

instance Show StaticType where
    show TypeBoolean = "boolean"
    show TypeInt = "int"
    show TypeVoid = "void"
    show (TypeObject o) = show o

instance (Show ref, Show info) => Show (SSAProgram info ref) where
    show (SSAProgram _ ss cs) = printf "program:\n  main:\n    method main:\n%s%s" (concatMap show ss) (concatMap show cs)

instance (Show ref, Show info) => Show (SSAMethod info ref) where
    show (SSAMethod (AST.MethodDecl _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s" name (concatMap show ss)

instance (Show ref, Show info) => Show (SSAClass info ref) where
    show (SSAClass (AST.ClassDecl name _ _ _) _ ms) = printf "  class %s:\n%s" name (concatMap show ms)

instance Show info => Show (SSAField info) where
    show (SSAField (AST.VarDecl _ name) _ _) = name

instance (Show ref, Show info) => Show (SSAStatement info ref) where
    show (SSAStatement id op info) = printf "      %s: %s :%s\n" (show id) (show op) (show info)

instance Show ref => Show (SSAOp ref) where
    show (Unify l r) = printf "Unify %s %s" (show l) (show r)
    show (Alias s) = printf "Alias %s" (show s)
    show This = printf "This"
    show (Parameter _ index) = printf "Parameter *%s" (show index)
    show (Arg arg index) = printf "Arg %s *%s" (show arg) (show index)
    show (Null AST.BooleanType) = printf "Null *Type(boolean)"
    show (Null AST.IntType) = printf "Null *Type(int)"
    show (Null AST.IntArrayType) = printf "Null *Type(int[])"
    show (Null (AST.ObjectType name)) = printf "Null *Type(%s)" name
    show (SInt v) = printf "Int *%s" (show v)
    show (SBoolean v) = printf "Boolean *%s" (if v then "true" else "false")
    show (NewObj name) = printf "NewObj *%s" name
    show (NewIntArray s) = printf "NewIntArray *%s" (show s)
    show (Label label) = printf "Label *%s" label
    show (Goto label) = printf "Goto *%s" label
    show (Branch s label) = printf "Branch %s *%s" (show s) label
    show (NBranch s label) = printf "NBranch %s *%s" (show s) label
    show (Call name s args) = printf "Call %s *%s(%s)" (show s) name (intercalate ", " $ map show args)
    show (Print s) = printf "Print %s" (show s)
    show (Return s) = printf "Return %s" (show s)
    show (Member s name) = printf "Member %s *%s" (show s) name
    show (Index a i) = printf "Index %s %s" (show a) (show i)
    show (Store s i) = printf "Store %s *%s" (show s) (show i)
    show (Load i) = printf "Load *%s" (show i)
    show (VarAssg s name) = printf "VarAssg %s *%s" (show s) name
    show (MemberAssg object value name) = printf "MemberAssg %s %s *%s" (show object) (show value) name
    show (IndexAssg array value index) = printf "IndexAssg %s %s *%s" (show array) (show value) (show index)
    show (Not s) = printf "Not %s" (show s)
    show (Lt sl sr) = printf "Lt %s %s" (show sl) (show sr)
    show (Le sl sr) = printf "Le %s %s" (show sl) (show sr)
    show (Eq sl sr) = printf "Eq %s %s" (show sl) (show sr)
    show (Ne sl sr) = printf "Ne %s %s" (show sl) (show sr)
    show (Gt sl sr) = printf "Gt %s %s" (show sl) (show sr)
    show (Ge sl sr) = printf "Ge %s %s" (show sl) (show sr)
    show (And sl sr) = printf "And %s %s" (show sl) (show sr)
    show (Or sl sr) = printf "Or %s %s" (show sl) (show sr)
    show (Plus sl sr) = printf "Plus %s %s" (show sl) (show sr)
    show (Minus sl sr) = printf "Minus %s %s" (show sl) (show sr)
    show (Mul sl sr) = printf "Mul %s %s" (show sl) (show sr)
    show (Div sl sr) = printf "Div %s %s" (show sl) (show sr)
    show (Mod sl sr) = printf "Mod %s %s" (show sl) (show sr)

instance Bifunctor SSAProgram where
    bimap f g (SSAProgram ast rs cs) = SSAProgram ast (map g rs) (map (bimap f g) cs)

instance Bifunctor SSAClass where
    bimap f g (SSAClass cd fs ms) = SSAClass cd (map (fmap f) fs) (map (bimap f g) ms)

instance Bifunctor SSAMethod where
    bimap f g (SSAMethod md ps ss r) = SSAMethod md (map g ps) (map g ss) (g r)

instance Bifunctor SSAStatement where
    bimap f g (SSAStatement id op info) = SSAStatement (g id) (fmap g op) (f info)

ssaCompile :: AST.Program -> (SSAProgram () ID, [ID], M.Map ID (SSAStatement () ID))
ssaCompile program = let (a, s) = runState scProgram state in (a, _stIDList s, _stIDToS s)
    where
    state = SSAState
        { _stProgram = program
        , _stNextID = 0
        , _stVarToID = M.empty
        , _stIDToS = M.empty
        , _stIDList = []
        , _stNextLabel = 0
        }

scProgram :: State (SSAState () ID) (SSAProgram () ID)
scProgram = do
    state <- get
    let program = state ^. stProgram
    let main = state ^. stProgram . AST.pMain
    let classes = state ^. stProgram . AST.pClassDecls
    SSAProgram program <$> (scStatement main >> get >>= return . (^. stIDList)) <*> (mapM scClassDecl classes)

scStatement :: AST.Statement -> State (SSAState () ID) ()
scStatement (AST.BlockStatement ss) = mapM scStatement ss >> return ()
scStatement axe@(AST.IfStatement condExp branchTrue branchFalse) = do
    condSSA <- scExp condExp

    labelElse <- (printf "l_%d" :: Int -> String) <$> nextLabel
    labelDone <- (printf "l_%d" :: Int -> String) <$> nextLabel

    buildStatement (NBranch condSSA labelElse)

    preBranchBindings <- (^. stVarToID) <$> get

    scStatement branchTrue
    buildStatement (Goto labelDone)
    buildStatement (Label labelElse)
    postTrueBindings <- (^. stVarToID) <$> get

    modify $ stVarToID .~ preBranchBindings
    case branchFalse of
        Just b -> scStatement b
        Nothing -> return ()
    postFalseBindings <- (^. stVarToID) <$> get

    buildStatement (Label labelDone)

    let bindings = M.assocs $ M.intersectionWith (,) postTrueBindings postFalseBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStatement . uncurry Unify . snd) mismatches
    zipWithM_ insertVarToID (map fst mismatches) unifies

    return ()
scStatement (AST.WhileStatement condExp body) = do
    labelStart <- (printf "l_%d" :: Int -> String) <$> nextLabel
    labelEnd <- (printf "l_%d" :: Int -> String) <$> nextLabel

    buildStatement (Label labelStart)

    preBranchState <- get >>= return
    let preBranchBindings = preBranchState ^. stVarToID

    condSSA <- scExp condExp

    buildStatement (NBranch condSSA labelEnd)

    scStatement body

    buildStatement (Goto labelStart)
    buildStatement (Label labelEnd)

    postBranchBindings <- (^. stVarToID) <$> get

    let bindings = M.assocs $ M.intersectionWith (,) preBranchBindings postBranchBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (buildStatement . uncurry Unify . snd) mismatches
    zipWithM_ insertVarToID (map fst mismatches) unifies

    return ()
scStatement (AST.PrintStatement e) = do
    value <- scExp e
    buildStatement (Print value)
    return ()
scStatement (AST.ExpressionStatement e) = singleton <$> scExp e >> return ()

scExp :: AST.Exp -> State (SSAState () ID) ID
scExp (AST.IntLiteral v) = buildStatement (SInt v)
scExp (AST.BooleanLiteral v) = buildStatement (SBoolean v)
scExp (AST.AssignExpression var val) = case var of
    AST.VarExp name -> do
        bs <- get >>= return . _stVarToID
        case M.lookup name bs of
            Just s -> do
                r <- scExp val
                v <- buildStatement (VarAssg r name)
                insertVarToID name v
                return v
            Nothing -> do
                this <- buildStatement This
                r <- scExp val
                buildStatement (MemberAssg this r name)
    AST.MemberExp objectExp fieldName -> do
        object <- scExp objectExp
        r <- scExp val
        buildStatement (MemberAssg object r fieldName)
    AST.IndexExp arrayExp indexExp -> do
        array <- scExp arrayExp
        index <- scExp indexExp
        r <- scExp val
        buildStatement (IndexAssg array index r)
    l -> error $ "Invalid LHS: " ++ show l
scExp (AST.BinaryExpression l op r) = do
    sl <- scExp l
    sr <- scExp r
    case lookup op binaryOps of
        Just opConstructor -> buildStatement (opConstructor sl sr)
        Nothing -> error $ "Op " ++ op ++ " not found in list: " ++ show (map fst binaryOps)
scExp (AST.NotExp e) = do
    r <- scExp e
    buildStatement (Not r)
scExp (AST.IndexExp arrayExp indexExp) = do
    array <- scExp arrayExp
    index <- scExp indexExp
    buildStatement (Index array index)
scExp (AST.CallExp objectExp methodName argExps) = do
    object <- scExp objectExp
    let makeArg argExp i = do
        target <- scExp argExp
        buildStatement (Arg target i)
    args <- zipWithM makeArg argExps [0 .. ]
    buildStatement (Call methodName object args)
scExp (AST.MemberExp objectExp fieldName) = do
    object <- scExp objectExp
    buildStatement (Member object fieldName)
scExp (AST.VarExp name) = do
    bs <- get >>= return . _stVarToID
    case M.lookup name bs of
        Just s -> return s
        Nothing -> do
            this <- buildStatement This
            buildStatement (Member this name)
scExp (AST.NewIntArrayExp index) = do
    r <- scExp index
    buildStatement (NewIntArray r)
scExp (AST.NewObjectExp name) = do
    buildStatement (NewObj name)
scExp AST.ThisExp = buildStatement This

scClassDecl :: AST.ClassDecl -> State (SSAState () ID) (SSAClass () ID)
scClassDecl ast@(AST.ClassDecl name extends vs ms) =
    SSAClass ast <$> zipWithM scVarDeclAsField vs [0 .. ] <*> mapM scMethodDecl ms

scVarDeclAsField :: AST.VarDecl -> Int -> State (SSAState () ID) (SSAField ())
scVarDeclAsField v i = return $ SSAField v i ()

scMethodDecl :: AST.MethodDecl -> State (SSAState () ID) (SSAMethod () ID)
scMethodDecl ast@(AST.MethodDecl t name ps vs ss ret) = do
    modify $ stIDList .~ []
    ssaParams <- mapM buildStatement (zipWith Parameter ps [0 .. ])
    ssaVarAssgs <- mapM buildStatement $ zipWith VarAssg ssaParams (map (^. AST.parName) ps)
    sequence $ zipWith insertVarToID (map (^. AST.parName) ps) ssaVarAssgs
    mapM scVarDecl vs
    mapM scStatement ss
    ret' <- scExp ret
    buildStatement (Return ret')
    allStatements <- (^. stIDList) <$> get
    return $ SSAMethod ast ssaParams allStatements ret'

scVarDecl :: AST.VarDecl -> State (SSAState () ID) ID
scVarDecl (AST.VarDecl t name) = do
    r <- buildStatement (Null t)
    modify $ stVarToID %~ M.insert name r
    return r

singleton :: a -> [a]
singleton a = [a]

binaryOps :: [(String, ref -> ref -> SSAOp ref)]
binaryOps =
    [ ("<", Lt)
    , ("<=", Le)
    , ("==", Eq)
    , ("!=", Ne)
    , (">", Gt)
    , (">=", Ge)
    , ("&&", And)
    , ("||", Or)
    , ("+", Plus)
    , ("-", Minus)
    , ("*", Mul)
    , ("/", Div)
    , ("%", Mod)
    ]

nextID :: State (SSAState () ID) ID
nextID = state $ \s -> (s ^. stNextID, stNextID %~ succ $ s)

nextLabel :: State (SSAState () ID) Int
nextLabel = state $ \s -> (s ^. stNextLabel, stNextLabel %~ succ $ s)

buildStatement :: SSAOp ID -> State (SSAState () ID) ID
buildStatement op = do
    id <- nextID
    modify $ stIDList %~ (++ [id])
    modify $ stIDToS %~ M.insert id (SSAStatement id op ())
    return id

insertVarToID :: String -> ID -> State (SSAState info ID) ()
insertVarToID name id = modify $ stVarToID %~ M.insert name id
