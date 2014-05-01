{-# LANGUAGE DeriveFunctor #-}

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

data SSAProgram ref info = SSAProgram AST.Program [ref] [SSAClass ref info]

data SSAClass ref info = SSAClass AST.ClassDecl [SSAField info] [SSAMethod ref info]

data SSAField info = SSAField AST.VarDecl Int info deriving (Functor)

data SSAMethod ref info = SSAMethod AST.MethodDecl [ref] [ref] ref

data StaticType =
      TypeInt
    | TypeBoolean
    | TypeVoid
    | TypeObject StaticTypeObject deriving (Show)

data StaticTypeObject = StaticTypeObject
    { typeName :: String
    , typeSuper :: (Maybe StaticTypeObject)
    }

data SSAStatement ref info = SSAStatement { getIDSSA :: ID, getOp :: SSAOp ref, getInfo :: info } deriving (Eq)

type ID = Int

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

instance Show StaticTypeObject where
    show (StaticTypeObject name Nothing) = name
    show (StaticTypeObject name (Just super)) = name ++ " <" ++ show super

instance (Show ref, Show info) => Show (SSAProgram ref info) where
    show (SSAProgram _ ss cs) = printf "program:\n  main:\n    method main:\n%s%s" (concatMap show ss) (concatMap show cs)

instance (Show ref, Show info) => Show (SSAMethod ref info) where
    show (SSAMethod (AST.MethodDecl _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s" name (concatMap show ss)

instance (Show ref, Show info) => Show (SSAClass ref info) where
    show (SSAClass (AST.ClassDecl name _ _ _) _ ms) = printf "  class %s:\n%s" name (concatMap show ms)

instance Show info => Show (SSAField info) where
    show (SSAField (AST.VarDecl _ name) _ _) = name

instance (Show ref, Show info) => Show (SSAStatement ref info) where
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
    bimap f g (SSAProgram ast rs cs) = SSAProgram ast (map f rs) (map (bimap f g) cs)

instance Bifunctor SSAClass where
    bimap f g (SSAClass cd fs ms) = SSAClass cd (map (fmap g) fs) (map (bimap f g) ms)

instance Bifunctor SSAMethod where
    bimap f g (SSAMethod md ps ss r) = SSAMethod md (map f ps) (map f ss) (f r)

-- instance Show StaticType where
--     show TypeInt = "Type(int)"
--     show TypeBoolean = "Type(boolean)"
--     show (TypeObject t) = show t
--
-- instance Show StaticTypeObject where
--     show (StaticTypeObject name _) = printf "Type(%s)" name

data SSAState ref info = SSAState
    { getProg     :: AST.Program
    , getID       :: ID
    , getBindings :: M.Map String ID
    , getIDMap    :: M.Map ID (SSAStatement ref info)
    , getSSAList  :: [ID]
    , getLabel    :: Int
    }

sInfo :: Show info => SSAStatement ref info -> String
sInfo (SSAStatement { getInfo = info }) = show info

opConstructors =
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

nextID :: State (SSAState ID ()) ID
nextID = do
    s@(SSAState { getID = id }) <- get
    put (s { getID = succ id })
    return id

nextLabel :: State (SSAState ID ()) Int
nextLabel = do
    s@(SSAState { getLabel = l }) <- get
    put (s { getLabel = succ l })
    return l

make :: SSAOp ID -> State (SSAState ID ()) ID
make op = do
    s@(SSAState { getID = id, getSSAList = ss, getIDMap = idMap}) <- get
    let newStatement = SSAStatement id op ()
    put (s { getID = succ id, getSSAList = ss ++ [id], getIDMap = M.insert id newStatement idMap })
    return id

insertBinding :: String -> ID -> State (SSAState ID info) ()
insertBinding name id = do
    s@(SSAState { getBindings = bs }) <- get
    put (s { getBindings = M.insert name id bs })

getSSA :: ID -> State (SSAState ID info) (SSAStatement ID info)
getSSA id = do
    SSAState { getIDMap = idMap } <- get
    fromJust <$> return (M.lookup id idMap)

singleton a = [a]

ssaCompile :: AST.Program -> (SSAProgram ID (), M.Map ID (SSAStatement ID ()))
ssaCompile program = let (a, s) = runState scProgram state in (a, getIDMap s)
    where
    state = SSAState
        { getProg = program
        , getID = 0
        , getBindings = M.empty
        , getIDMap = M.empty
        , getSSAList = []
        , getLabel = 0
        }

scProgram :: State (SSAState ID ()) (SSAProgram ID ())
scProgram = do
    SSAState { getProg = ast@(AST.Program s cs) } <- get
    SSAProgram ast <$> (scStatement s >> get >>= return . getSSAList) <*> (mapM scClassDecl cs)

scStatement :: AST.Statement -> State (SSAState ID ()) ()
scStatement (AST.BlockStatement ss) = mapM scStatement ss >> return ()
scStatement axe@(AST.IfStatement condExp branchTrue branchFalse) = do
    condSSA <- scExp condExp

    elseN <- nextLabel
    doneN <- nextLabel
    let labelElse = printf "l_%d" elseN :: String
    let labelDone = printf "l_%d" doneN :: String

    make (NBranch condSSA labelElse)

    preBranchState@(SSAState { getBindings = preBranchBindings }) <- get

    scStatement branchTrue
    make (Goto labelDone)
    make (Label labelElse)
    postTrueState@(SSAState { getBindings = postTrueBindings }) <- get

    put (postTrueState { getBindings = preBranchBindings })
    case branchFalse of
        Just b -> scStatement b
        Nothing -> return ()
    postFalseState@(SSAState { getBindings = postFalseBindings }) <- get

    make (Label labelDone)

    let bindings = M.assocs $ M.intersectionWith (,) postTrueBindings postFalseBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (make . uncurry Unify . snd) mismatches
    zipWithM_ insertBinding (map fst mismatches) unifies

    return ()
scStatement (AST.WhileStatement condExp body) = do
    startN <- nextLabel
    endN <- nextLabel
    let labelStart = printf "l_%d" startN :: String
    let labelEnd = printf "l_%d" endN :: String

    make (Label labelStart)

    preBranchState@(SSAState { getBindings = preBranchBindings }) <- get

    condSSA <- scExp condExp

    make (NBranch condSSA labelEnd)

    scStatement body

    make (Goto labelStart)
    make (Label labelEnd)

    postBranchState@(SSAState { getBindings = postBranchBindings }) <- get

    let bindings = M.assocs $ M.intersectionWith (,) preBranchBindings postBranchBindings
    let mismatches = filter (uncurry (/=) . snd) bindings
    unifies <- mapM (make . uncurry Unify . snd) mismatches
    zipWithM_ insertBinding (map fst mismatches) unifies

    return ()
scStatement (AST.PrintStatement e) = do
    value <- scExp e
    make (Print value)
    return ()
scStatement (AST.ExpressionStatement e) = singleton <$> scExp e >> return ()

scExp :: AST.Exp -> State (SSAState ID ()) ID
scExp (AST.IntLiteral v) = make (SInt v)
scExp (AST.BooleanLiteral v) = make (SBoolean v)
scExp (AST.AssignExpression var val) = case var of
    AST.VarExp name -> do
        bs <- get >>= return . getBindings
        case M.lookup name bs of
            Just s -> do
                r <- scExp val
                v <- make (VarAssg r name)
                insertBinding name v
                return v
            Nothing -> do
                this <- make This
                r <- scExp val
                make (MemberAssg this r name)
    AST.MemberExp objectExp fieldName -> do
        object <- scExp objectExp
        r <- scExp val
        make (MemberAssg object r fieldName)
    AST.IndexExp arrayExp indexExp -> do
        array <- scExp arrayExp
        index <- scExp indexExp
        r <- scExp val
        make (IndexAssg array index r)
    l -> error $ "Invalid LHS: " ++ show l
scExp (AST.BinaryExpression l op r) = do
    sl <- scExp l
    sr <- scExp r
    case lookup op opConstructors of
        Just opConstructor -> make (opConstructor sl sr)
        Nothing -> error $ "Op " ++ op ++ " not found in list: " ++ show (map fst opConstructors)
scExp (AST.NotExp e) = do
    r <- scExp e
    make (Not r)
scExp (AST.IndexExp arrayExp indexExp) = do
    array <- scExp arrayExp
    index <- scExp indexExp
    make (Index array index)
scExp (AST.CallExp objectExp methodName argExps) = do
    object <- scExp objectExp
    let makeArg argExp i = do
        target <- scExp argExp
        make (Arg target i)
    args <- zipWithM makeArg argExps [0 .. ]
    make (Call methodName object args)
scExp (AST.MemberExp objectExp fieldName) = do
    object <- scExp objectExp
    make (Member object fieldName)
scExp (AST.VarExp name) = do
    bs <- get >>= return . getBindings
    case M.lookup name bs of
        Just s -> return s
        Nothing -> do
            this <- make This
            make (Member this name)
scExp (AST.NewIntArrayExp index) = do
    r <- scExp index
    make (NewIntArray r)
scExp (AST.NewObjectExp name) = do
    make (NewObj name)
scExp (AST.ThisExp) = make This

scClassDecl :: AST.ClassDecl -> State (SSAState ID ()) (SSAClass ID ())
scClassDecl ast@(AST.ClassDecl name extends vs ms) =
    SSAClass ast <$> zipWithM scVarDeclAsField vs [0 .. ] <*> mapM scMethodDecl ms

scVarDeclAsField :: AST.VarDecl -> Int -> State (SSAState ID ()) (SSAField ())
scVarDeclAsField v i = return $ SSAField v i ()

scMethodDecl :: AST.MethodDecl -> State (SSAState ID ()) (SSAMethod ID ())
scMethodDecl ast@(AST.MethodDecl t name ps vs ss ret) = do
    s <- get
    put (s { getSSAList = [] })
    ssaParams <- mapM make (zipWith Parameter ps [0 .. ])
    ssaVarAssgs <- mapM make $ map (\((AST.Parameter _ name), p) -> VarAssg p name) $ zip ps ssaParams
    sequence $ zipWith insertBinding (map (\(AST.Parameter _ name) -> name) ps) (ssaVarAssgs)
    mapM scVarDecl vs
    mapM scStatement ss
    ret' <- scExp ret
    make (Return ret')
    SSAState { getSSAList = allStatements } <- get
    return $ SSAMethod ast ssaParams allStatements ret'

scVarDecl :: AST.VarDecl -> State (SSAState ID ()) ID
scVarDecl (AST.VarDecl t name) = do
    r <- make (Null t)
    s@(SSAState { getBindings = bs }) <- get
    put (s { getBindings = M.insert name r bs })
    return r
