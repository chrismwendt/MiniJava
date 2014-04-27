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

data SSAParameter info = SSAParameter AST.Parameter Int info

data SSAArgument info = SSAArgument (SSAStatement info) Int info deriving (Show)

data SSAReturn info = SSAReturn (SSAStatement info) deriving (Show)

data StaticType =
      TypeInt
    | TypeBoolean
    | TypeObject StaticTypeObject deriving (Show)

data StaticTypeObject = StaticTypeObject String (Maybe StaticTypeObject) deriving (Show)

data SSAStatement info = SSAStatement { getOp :: SSAOp info, getInfo :: info }

data SSAOp info =
      Unify (SSAStatement info) (SSAStatement info)
    | Alias (SSAStatement info)
    | This
    | Parameter (SSAParameter info)
    | Arg (SSAArgument info)
    | Null AST.Type
    | SInt Int
    | SBoolean Bool
    | NewObj String
    | NewIntArray (SSAStatement info)
    | Label String
    | Goto String
    | Branch (SSAStatement info) String
    | NBranch (SSAStatement info) String
    | Call AST.MethodDecl (SSAStatement info) [SSAArgument info]
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
    show (SSAMethod (AST.MethodDecl _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s%s" name (concatMap show ps) (concatMap show ss)

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
    show (Parameter (SSAParameter _ index _)) = printf "Parameter *%s" (show index)
    show (Arg (SSAArgument _ index info)) = printf "Arg %s *%s" (show info) (show index)
    show (Null t) = printf "Null *Type(%s)" (show t)
    show (SInt v) = printf "Int *%s" (show v)
    show (SBoolean v) = printf "Boolean *%s" (if v then "true" else "false")
    show (NewObj name) = printf "NewObj *%s" name
    show (NewIntArray s) = printf "NewIntArray *%s" (sInfo s)
    show (Label label) = printf "Label *%s" label
    show (Goto label) = printf "Goto *%s" label
    show (Branch s label) = printf "Branch %s *%s" (sInfo s) label
    show (NBranch s label) = printf "NBranch %s *%s" (sInfo s) label
    show (Call (AST.MethodDecl _ name _ _ _ _) s args) = printf "Call %s *%s(%s)" (sInfo s) name (intercalate ", " $ map sArgInfo args)
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
    show p@(SSAParameter _ _ info) = show (SSAStatement (Parameter p) info)

-- instance Show StaticType where
--     show TypeInt = "Type(int)"
--     show TypeBoolean = "Type(boolean)"
--     show (TypeObject t) = show t
--
-- instance Show StaticTypeObject where
--     show (StaticTypeObject name _) = printf "Type(%s)" name

data SSAState info = SSAState { getProg :: AST.Program, getID :: Int, getBindings :: M.Map String (SSAStatement info) }

sArgInfo :: Show info => SSAArgument info -> String
sArgInfo (SSAArgument _ _ info) = show info

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

nextID = do
    s@(SSAState { getID = id }) <- get
    put (s { getID = succ id })
    return id

ssaCompile :: AST.Program -> SSAProgram Int
ssaCompile program = evalState scProgram (SSAState { getProg = program, getID = 0, getBindings = M.empty })

scProgram :: State (SSAState Int) (SSAProgram Int)
scProgram = do
    SSAState { getProg = ast@(AST.Program s cs) } <- get
    SSAProgram ast <$> scStatement s <*> (mapM scClassDecl cs)

scStatement :: AST.Statement -> State (SSAState Int) [SSAStatement Int]
scStatement (AST.BlockStatement ss) = concat <$> mapM scStatement ss
-- scStatement (AST.IfStatement cond true) = scExp e
-- scStatement (AST.WhileStatement e) = scExp e
scStatement (AST.PrintStatement e) = do
    (ss, r) <- scExp e
    id <- nextID
    return (ss ++ [SSAStatement (Print r) id])
scStatement (AST.ExpressionStatement e) = fst <$> scExp e

scExp :: AST.Exp -> State (SSAState Int) ([SSAStatement Int], SSAStatement Int)
scExp (AST.IntLiteral v) = do
    id <- nextID
    let r = SSAStatement (SInt v) id
    return ([r], r)
scExp (AST.BooleanLiteral v) = do
    id <- nextID
    let r = SSAStatement (SBoolean v) id
    return ([r], r)
scExp (AST.BinaryExpression l op r) = do
    (ssl, sl) <- scExp l
    (ssr, sr) <- scExp r
    id <- nextID
    return (case lookup op opConstructors of
        Just opConstructor -> let final = SSAStatement (opConstructor sl sr) id in (ssl ++ ssr ++ [final], final)
        Nothing -> error $ "Op " ++ op ++ " not found in list: " ++ show (map fst opConstructors))
scExp (AST.NotExp e) = do
    (ss, r) <- scExp e
    id <- nextID
    let final = SSAStatement (Not r) id
    return $ (ss ++ [final], final)

scClassDecl :: AST.ClassDecl -> State (SSAState Int) (SSAClass Int)
scClassDecl ast@(AST.ClassDecl name extends vs ms) = SSAClass ast <$> mapM scVarDeclAsField (zip vs [0 .. ]) <*> mapM scMethodDecl ms

scVarDeclAsField :: (AST.VarDecl, Int) -> State (SSAState Int) (SSAField Int)
scVarDeclAsField (v, i) = return $ SSAField v i 0

scMethodDecl :: AST.MethodDecl -> State (SSAState Int) (SSAMethod Int)
scMethodDecl ast@(AST.MethodDecl t name ps vs ss ret) = do
    ps' <- mapM scParameter (zip ps [0 .. ])
    vs' <- mapM scVarDecl vs
    ss' <- concat <$> mapM scStatement ss
    (rs, ret') <- scExp ret
    return $ SSAMethod ast ps' (vs' ++ ss' ++ rs) (SSAReturn ret')

scParameter :: (AST.Parameter, Int) -> State (SSAState Int) (SSAParameter Int)
scParameter (ast, i) = SSAParameter ast <$> pure i <*> nextID

scVarDecl :: AST.VarDecl -> State (SSAState Int) (SSAStatement Int)
scVarDecl (AST.VarDecl t name) = do
    id <- nextID
    let r = SSAStatement (Null t) id
    s@(SSAState { getBindings = bs }) <- get
    put (s { getBindings = M.insert name r bs })
    return r
