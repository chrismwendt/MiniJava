{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module SSACompiler where

import qualified AST
import qualified ASTUntyped
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
    { _pProgram :: ASTUntyped.Program
    , _pMain :: [ref]
    , _pClasses :: [SSAClass info ref]
    }

data SSAClass info ref = SSAClass
    { _cClass :: ASTUntyped.Class
    , _cFields :: [SSAField info]
    , _cMethod :: [SSAMethod info ref]
    }

data SSAField info = SSAField
    { _fVariable :: AST.Variable
    , _fPosition :: Int
    , _fInfo :: info
    }
    deriving (Functor)

data SSAMethod info ref = SSAMethod
    { _mMethod :: ASTUntyped.Method
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
    { _sOp :: SSAOp ref
    , _sInfo :: info
    }
    deriving (Eq)

data SSAOp ref =
      Unify ref ref
    | Alias ref
    | This
    | Variable AST.Variable Int
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
    | MemberGet ref String
    | IndexGet ref ref
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
    { _stProgram :: ASTUntyped.Program
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
    show (SSAMethod (ASTUntyped.Method _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s" name (concatMap show ss)

instance (Show ref, Show info) => Show (SSAClass info ref) where
    show (SSAClass (ASTUntyped.Class name _ _ _) _ ms) = printf "  class %s:\n%s" name (concatMap show ms)

instance Show info => Show (SSAField info) where
    show (SSAField (AST.Variable _ name) _ _) = name

instance (Show ref, Show info) => Show (SSAStatement info ref) where
    show (SSAStatement op info) = printf "      ?: %s :%s\n" (show op) (show info)

instance Show ref => Show (SSAOp ref) where
    show (Unify l r) = printf "Unify %s %s" (show l) (show r)
    show (Alias s) = printf "Alias %s" (show s)
    show This = printf "This"
    show (Variable _ index) = printf "Variable *%s" (show index)
    show (Arg arg index) = printf "Arg %s *%s" (show arg) (show index)
    show (Null AST.TypeBoolean) = printf "Null *Type(boolean)"
    show (Null AST.TypeInt) = printf "Null *Type(int)"
    show (Null AST.TypeIntArray) = printf "Null *Type(int[])"
    show (Null (AST.TypeObject name)) = printf "Null *Type(%s)" name
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
    show (MemberGet s name) = printf "Member %s *%s" (show s) name
    show (IndexGet a i) = printf "IndexGet %s %s" (show a) (show i)
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
    bimap f g (SSAStatement op info) = SSAStatement (fmap g op) (f info)

ssaCompile :: ASTUntyped.Program -> (SSAProgram () ID, [ID], M.Map ID (SSAStatement () ID))
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
    let main = state ^. stProgram . ASTUntyped.pMain
    let classes = state ^. stProgram . ASTUntyped.pClasses
    SSAProgram program <$> (scStatement main >> get >>= return . (^. stIDList)) <*> (mapM scClass classes)

scStatement :: ASTUntyped.Statement -> State (SSAState () ID) ()
scStatement (ASTUntyped.Block ss) = mapM scStatement ss >> return ()
scStatement axe@(ASTUntyped.If cond branchTrue branchFalse) = do
    condSSA <- sc cond

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
scStatement (ASTUntyped.While cond body) = do
    labelStart <- (printf "l_%d" :: Int -> String) <$> nextLabel
    labelEnd <- (printf "l_%d" :: Int -> String) <$> nextLabel

    buildStatement (Label labelStart)

    preBranchState <- get >>= return
    let preBranchBindings = preBranchState ^. stVarToID

    condSSA <- sc cond

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
scStatement (ASTUntyped.Print e) = do
    value <- sc e
    buildStatement (Print value)
    return ()
scStatement (ASTUntyped.ExpressionStatement e) = singleton <$> sc e >> return ()

sc :: ASTUntyped.Expression -> State (SSAState () ID) ID
sc (ASTUntyped.LiteralInt v) = buildStatement (SInt v)
sc (ASTUntyped.LiteralBoolean v) = buildStatement (SBoolean v)
sc (ASTUntyped.Assignment var val) = case var of
    ASTUntyped.VariableGet name -> do
        bs <- get >>= return . _stVarToID
        case M.lookup name bs of
            Just s -> do
                r <- sc val
                v <- buildStatement (VarAssg r name)
                insertVarToID name v
                return v
            Nothing -> do
                this <- buildStatement This
                r <- sc val
                buildStatement (MemberAssg this r name)
    ASTUntyped.MemberGet object fieldName -> do
        object <- sc object
        r <- sc val
        buildStatement (MemberAssg object r fieldName)
    ASTUntyped.IndexGet array index -> do
        array <- sc array
        index <- sc index
        r <- sc val
        buildStatement (IndexAssg array index r)
    l -> error $ "Invalid LHS: " ++ show l
sc (ASTUntyped.Binary l op r) = do
    sl <- sc l
    sr <- sc r
    case lookup op binaryOps of
        Just opConstructor -> buildStatement (opConstructor sl sr)
        Nothing -> error $ "Op " ++ show op ++ " not found in list: " ++ show (map fst binaryOps)
sc (ASTUntyped.Not e) = do
    r <- sc e
    buildStatement (Not r)
sc (ASTUntyped.IndexGet array index) = do
    array <- sc array
    index <- sc index
    buildStatement (IndexGet array index)
sc (ASTUntyped.Call object methodName argExps) = do
    object <- sc object
    let makeArg arg i = do
        target <- sc arg
        buildStatement (Arg target i)
    args <- zipWithM makeArg argExps [0 .. ]
    buildStatement (Call methodName object args)
sc (ASTUntyped.MemberGet object fieldName) = do
    object <- sc object
    buildStatement (MemberGet object fieldName)
sc (ASTUntyped.VariableGet name) = do
    bs <- get >>= return . _stVarToID
    case M.lookup name bs of
        Just s -> return s
        Nothing -> do
            this <- buildStatement This
            buildStatement (MemberGet this name)
sc (ASTUntyped.NewIntArray index) = do
    r <- sc index
    buildStatement (NewIntArray r)
sc (ASTUntyped.NewObject name) = do
    buildStatement (NewObj name)
sc ASTUntyped.This = buildStatement This

scClass :: ASTUntyped.Class -> State (SSAState () ID) (SSAClass () ID)
scClass ast@(ASTUntyped.Class name extends vs ms) =
    SSAClass ast <$> zipWithM scVariableAsField vs [0 .. ] <*> mapM scMethod ms

scVariableAsField :: AST.Variable -> Int -> State (SSAState () ID) (SSAField ())
scVariableAsField v i = return $ SSAField v i ()

scMethod :: ASTUntyped.Method -> State (SSAState () ID) (SSAMethod () ID)
scMethod ast@(ASTUntyped.Method t name ps vs ss ret) = do
    modify $ stIDList .~ []
    ssaParams <- mapM buildStatement (zipWith Variable ps [0 .. ])
    ssaVarAssgs <- mapM buildStatement $ zipWith VarAssg ssaParams (map (^. AST.vName) ps)
    sequence $ zipWith insertVarToID (map (^. AST.vName) ps) ssaVarAssgs
    mapM scVariable vs
    mapM scStatement ss
    ret' <- sc ret
    buildStatement (Return ret')
    allStatements <- (^. stIDList) <$> get
    return $ SSAMethod ast ssaParams allStatements ret'

scVariable :: AST.Variable -> State (SSAState () ID) ID
scVariable (AST.Variable t name) = do
    r <- buildStatement (Null t)
    modify $ stVarToID %~ M.insert name r
    return r

singleton :: a -> [a]
singleton a = [a]

binaryOps :: [(AST.BinaryOperator, ref -> ref -> SSAOp ref)]
binaryOps =
    [ (AST.Lt, Lt)
    , (AST.Le, Le)
    , (AST.Eq, Eq)
    , (AST.Ne, Ne)
    , (AST.Gt, Gt)
    , (AST.Ge, Ge)
    , (AST.And, And)
    , (AST.Or, Or)
    , (AST.Plus, Plus)
    , (AST.Minus, Minus)
    , (AST.Mul, Mul)
    , (AST.Div, Div)
    , (AST.Mod, Mod)
    ]

nextID :: State (SSAState () ID) ID
nextID = state $ \s -> (s ^. stNextID, stNextID %~ succ $ s)

nextLabel :: State (SSAState () ID) Int
nextLabel = state $ \s -> (s ^. stNextLabel, stNextLabel %~ succ $ s)

buildStatement :: SSAOp ID -> State (SSAState () ID) ID
buildStatement op = do
    id <- nextID
    modify $ stIDList %~ (++ [id])
    modify $ stIDToS %~ M.insert id (SSAStatement op ())
    return id

insertVarToID :: String -> ID -> State (SSAState info ID) ()
insertVarToID name id = modify $ stVarToID %~ M.insert name id
