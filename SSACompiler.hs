module SSACompiler where

import qualified AST
import Text.Printf
import Data.List
import Data.Functor
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M

data SSAProgram info = SSAProgram AST.Program [SSAStatement info] [SSAClass info]

data SSAClass info = SSAClass AST.ClassDecl [SSAField info] [SSAMethod info]

data SSAField info = SSAField AST.VarDecl Int info

data SSAMethod info = SSAMethod AST.MethodDecl [SSAParameter info] [SSAStatement info] (SSAReturn info)

data SSAParameter info = SSAParameter (SSAStatement info)

data SSAArgument info = SSAArgument (SSAStatement info) deriving (Show)

data SSAReturn info = SSAReturn (SSAStatement info) deriving (Show)

data StaticType =
      TypeInt
    | TypeBoolean
    | TypeObject StaticTypeObject deriving (Show)

data StaticTypeObject = StaticTypeObject String (Maybe StaticTypeObject) deriving (Show)

data SSAStatement info = SSAStatement { getOp :: SSAOp info, getInfo :: info }

-- TODO remove SSAParameter, SSAReturn, SSAArgument
data SSAOp info =
      Unify (SSAStatement info) (SSAStatement info)
    | Alias (SSAStatement info)
    | This
    | Parameter AST.Parameter Int
    | Arg (SSAStatement info) Int
    | Null AST.Type
    | SInt Int
    | SBoolean Bool
    | NewObj String
    | NewIntArray (SSAStatement info)
    | Label String
    | Goto String
    | Branch (SSAStatement info) String
    | NBranch (SSAStatement info) String
    | Call String (SSAStatement info) [SSAArgument info]
    | Print (SSAStatement info)
    | Return (SSAStatement info)
    | Member (SSAStatement info) String
    | Index (SSAStatement info) (SSAStatement info)
    | Store (SSAStatement info) Int
    | Load Int
    | VarAssg (SSAStatement info) String
    | MemberAssg (SSAStatement info) (SSAStatement info) String
    | IndexAssg (SSAStatement info) (SSAStatement info) (SSAStatement info)
    | Not (SSAStatement info)
    | Lt (SSAStatement info) (SSAStatement info)
    | Le (SSAStatement info) (SSAStatement info)
    | Eq (SSAStatement info) (SSAStatement info)
    | Ne (SSAStatement info) (SSAStatement info)
    | Gt (SSAStatement info) (SSAStatement info)
    | Ge (SSAStatement info) (SSAStatement info)
    | And (SSAStatement info) (SSAStatement info)
    | Or (SSAStatement info) (SSAStatement info)
    | Plus (SSAStatement info) (SSAStatement info)
    | Minus (SSAStatement info) (SSAStatement info)
    | Mul (SSAStatement info) (SSAStatement info)
    | Div (SSAStatement info) (SSAStatement info)
    | Mod (SSAStatement info) (SSAStatement info)

instance Show info => Show (SSAProgram info) where
    show (SSAProgram _ ss cs) = printf "program:\n  main:\n    method main:\n%s%s" (concatMap show ss) (concatMap show cs)

instance Show info => Show (SSAMethod info) where
    show (SSAMethod (AST.MethodDecl _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s" name (concatMap show ss)

instance Show info => Show (SSAClass info) where
    show (SSAClass (AST.ClassDecl name _ _ _) _ ms) = printf "  class %s:\n%s" name (concatMap show ms)

instance Show info => Show (SSAField info) where
    show (SSAField (AST.VarDecl _ name) _ _) = name

instance Show info => Show (SSAStatement info) where
    show (SSAStatement op info) = printf "      %s: %s\n" (show info) (show op)

instance Show info => Show (SSAOp info) where
    show (Unify l r) = printf "Unify %s %s" (sInfo l) (sInfo r)
    show (Alias s) = printf "Alias %s" (sInfo s)
    show This = printf "This"
    show (Parameter _ index) = printf "Parameter *%s" (show index)
    show (Arg arg index) = printf "Arg %s *%s" (sInfo arg) (show index)
    show (Null AST.BooleanType) = printf "Null *Type(boolean)"
    show (Null AST.IntType) = printf "Null *Type(int)"
    show (Null AST.IntArrayType) = printf "Null *Type(int[])"
    show (Null (AST.ObjectType name)) = printf "Null *Type(%s)" name
    show (SInt v) = printf "Int *%s" (show v)
    show (SBoolean v) = printf "Boolean *%s" (if v then "true" else "false")
    show (NewObj name) = printf "NewObj *%s" name
    show (NewIntArray s) = printf "NewIntArray *%s" (sInfo s)
    show (Label label) = printf "Label *%s" label
    show (Goto label) = printf "Goto *%s" label
    show (Branch s label) = printf "Branch %s *%s" (sInfo s) label
    show (NBranch s label) = printf "NBranch %s *%s" (sInfo s) label
    show (Call name s args) = printf "Call %s *%s(%s)" (sInfo s) name (intercalate ", " $ map sArgInfo args)
    show (Print s) = printf "Print %s" (sInfo s)
    show (Return s) = printf "Return %s" (sInfo s)
    show (Member s name) = printf "Member %s *%s" (sInfo s) name
    show (Index a i) = printf "Index %s %s" (sInfo a) (sInfo i)
    show (Store s i) = printf "Store %s *%s" (sInfo s) (show i)
    show (Load i) = printf "Load *%s" (show i)
    show (VarAssg s name) = printf "VarAssg %s *%s" (sInfo s) name
    show (MemberAssg object value name) = printf "MemberAssg %s %s *%s" (sInfo object) (sInfo value) name
    show (IndexAssg array value index) = printf "IndexAssg %s %s *%s" (sInfo array) (sInfo value) (sInfo index)
    show (Not s) = printf "Not %s" (sInfo s)
    show (Lt sl sr) = printf "Lt %s %s" (sInfo sl) (sInfo sr)
    show (Le sl sr) = printf "Le %s %s" (sInfo sl) (sInfo sr)
    show (Eq sl sr) = printf "Eq %s %s" (sInfo sl) (sInfo sr)
    show (Ne sl sr) = printf "Ne %s %s" (sInfo sl) (sInfo sr)
    show (Gt sl sr) = printf "Gt %s %s" (sInfo sl) (sInfo sr)
    show (Ge sl sr) = printf "Ge %s %s" (sInfo sl) (sInfo sr)
    show (And sl sr) = printf "And %s %s" (sInfo sl) (sInfo sr)
    show (Or sl sr) = printf "Or %s %s" (sInfo sl) (sInfo sr)
    show (Plus sl sr) = printf "Plus %s %s" (sInfo sl) (sInfo sr)
    show (Minus sl sr) = printf "Minus %s %s" (sInfo sl) (sInfo sr)
    show (Mul sl sr) = printf "Mul %s %s" (sInfo sl) (sInfo sr)
    show (Div sl sr) = printf "Div %s %s" (sInfo sl) (sInfo sr)
    show (Mod sl sr) = printf "Mod %s %s" (sInfo sl) (sInfo sr)

instance Show info => Show (SSAParameter info) where
    show (SSAParameter p) = show p

-- instance Show StaticType where
--     show TypeInt = "Type(int)"
--     show TypeBoolean = "Type(boolean)"
--     show (TypeObject t) = show t
--
-- instance Show StaticTypeObject where
--     show (StaticTypeObject name _) = printf "Type(%s)" name

data SSAState info = SSAState
    { getProg     :: AST.Program
    , getID       :: Int
    , getBindings :: M.Map String (SSAStatement info)
    , getSSAList  :: [SSAStatement info]
    , getLabel    :: Int
    }

sArgInfo :: Show info => SSAArgument info -> String
sArgInfo (SSAArgument s) = sInfo s

sInfo :: Show info => SSAStatement info -> String
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

nextID :: State (SSAState Int) Int
nextID = do
    s@(SSAState { getID = id }) <- get
    put (s { getID = succ id })
    return id

nextLabel :: State (SSAState Int) Int
nextLabel = do
    s@(SSAState { getLabel = l }) <- get
    put (s { getID = succ l })
    return l

make :: SSAOp Int -> State (SSAState Int) (SSAStatement Int)
make op = do
    s@(SSAState { getID = id, getSSAList = ss }) <- get
    let newStatement = SSAStatement op id
    put (s { getID = succ id, getSSAList = ss ++ [newStatement] })
    return newStatement

insertBinding :: String -> (SSAStatement info) -> State (SSAState info) ()
insertBinding name value = do
    s@(SSAState { getBindings = bs }) <- get
    put (s { getBindings = M.insert name value bs })

singleton a = [a]

ssaCompile :: AST.Program -> SSAProgram Int
ssaCompile program = evalState scProgram (SSAState { getProg = program, getID = 0, getBindings = M.empty, getSSAList = [], getLabel = 0 })

scProgram :: State (SSAState Int) (SSAProgram Int)
scProgram = do
    SSAState { getProg = ast@(AST.Program s cs) } <- get
    SSAProgram ast <$> (scStatement s >> get >>= return . getSSAList) <*> (mapM scClassDecl cs)

scStatement :: AST.Statement -> State (SSAState Int) ()
scStatement (AST.BlockStatement ss) = mapM scStatement ss >> return ()
-- scStatement (AST.IfStatement condExp branchTrue branchFalse) = do
--     condSSA <- sExp condExp
-- scStatement (AST.WhileStatement e) = scExp e
scStatement (AST.PrintStatement e) = do
    value <- scExp e
    make (Print value)
    return ()
scStatement (AST.ExpressionStatement e) = singleton <$> scExp e >> return ()

scExp :: AST.Exp -> State (SSAState Int) (SSAStatement Int)
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
    argTargets <- mapM scExp argExps
    args <- sequence $ zipWith (\a i -> make (Arg a i)) argTargets [0 .. ]
    make (Call methodName object (map SSAArgument args))
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
scExp a = error $ "Not implemented: " ++ (show a)

scClassDecl :: AST.ClassDecl -> State (SSAState Int) (SSAClass Int)
scClassDecl ast@(AST.ClassDecl name extends vs ms) = SSAClass ast <$> mapM scVarDeclAsField (zip vs [0 .. ]) <*> mapM scMethodDecl ms

scVarDeclAsField :: (AST.VarDecl, Int) -> State (SSAState Int) (SSAField Int)
scVarDeclAsField (v, i) = return $ SSAField v i 0

scMethodDecl :: AST.MethodDecl -> State (SSAState Int) (SSAMethod Int)
scMethodDecl ast@(AST.MethodDecl t name ps vs ss ret) = do
    -- TODO bind parameters in getBindings
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
    return $ SSAMethod ast (map SSAParameter ssaParams) allStatements (SSAReturn ret')

scParameter :: (AST.Parameter, Int) -> State (SSAState Int) (SSAParameter Int)
scParameter (ast, i) = do
    r <- make (Parameter ast i)
    return $ SSAParameter r

scVarDecl :: AST.VarDecl -> State (SSAState Int) (SSAStatement Int)
scVarDecl (AST.VarDecl t name) = do
    r <- make (Null t)
    s@(SSAState { getBindings = bs }) <- get
    put (s { getBindings = M.insert name r bs })
    return r
