module SSACompiler where

import qualified AST
import Text.Printf
import Data.List
import Data.Functor
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M

data SSAProgram info = SSAProgram AST.Program [SSAStatement info] [SSAClass info]

data SSAClass info = SSAClass AST.ClassDecl [SSAField info] [SSAMethod info] deriving (Show)

data SSAField info = SSAField AST.VarDecl Int info deriving (Show)

data SSAMethod info = SSAMethod AST.MethodDecl [SSAParameter info] [SSAStatement info] (SSAReturn info)

data SSAParameter info = SSAParameter AST.Parameter Int info deriving (Show)

data SSAArgument info = SSAArgument (SSAStatement info) Int info deriving (Show)

data SSAReturn info = SSAReturn (SSAStatement info) deriving (Show)

data StaticType =
      TypeInt
    | TypeBoolean
    | TypeObject StaticTypeObject deriving (Show)

data StaticTypeObject = StaticTypeObject String (Maybe StaticTypeObject) deriving (Show)

data SSAStatement info =
      Unify (SSAStatement info) (SSAStatement info) info
    | Alias (SSAStatement info) info
    | This info
    | Parameter (SSAParameter info) info
    | Arg (SSAArgument info) info
    | Null info
    | SInt Int info
    | SBoolean Bool info
    | NewObj String info
    | NewIntArray (SSAStatement info) info
    | Label String info
    | Goto String info
    | Branch (SSAStatement info) String info
    | NBranch (SSAStatement info) String info
    | Call AST.MethodDecl (SSAStatement info) [SSAArgument info] info
    | Print (SSAStatement info) info
    | Return (SSAStatement info) info
    | Member (SSAStatement info) String info
    | Index (SSAStatement info) (SSAStatement info) info
    | Store (SSAStatement info) Int info
    | Load Int info
    | VarAssg (SSAStatement info) String info
    | MemberAssg (SSAStatement info) (SSAStatement info) String info
    | IndexAssg (SSAStatement info) (SSAStatement info) (SSAStatement info) info
    | Not (SSAStatement info) info
    | Lt (SSAStatement info) (SSAStatement info) info
    | Le (SSAStatement info) (SSAStatement info) info
    | Eq (SSAStatement info) (SSAStatement info) info
    | Ne (SSAStatement info) (SSAStatement info) info
    | Gt (SSAStatement info) (SSAStatement info) info
    | Ge (SSAStatement info) (SSAStatement info) info
    | And (SSAStatement info) (SSAStatement info) info
    | Or (SSAStatement info) (SSAStatement info) info
    | Plus (SSAStatement info) (SSAStatement info) info
    | Minus (SSAStatement info) (SSAStatement info) info
    | Mul (SSAStatement info) (SSAStatement info) info
    | Div (SSAStatement info) (SSAStatement info) info
    | Mod (SSAStatement info) (SSAStatement info) info deriving (Show)

instance Show info => Show (SSAProgram info) where
    show (SSAProgram _ ss cs) = printf "program:\n  main:\n    method main:\n%s%s" (concatMap show ss) (concatMap show cs)

instance Show info => Show (SSAMethod info) where
    show (SSAMethod (AST.MethodDecl _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s%s" name (concatMap show ps) (concatMap show ss)

-- instance Show SSAClass where
--     show (SSAClass (AST.ClassDecl name _ _ _) fs ms) = printf "  class %s:\n%s" name (concatMap show ms)
--
-- instance Show SSAField where
--     show (SSAField vd name index t) = name
--
-- instance Show SSACall where
--     show (SSACall name ss) = printf "    method %s:\n%s" name (concatMap show ss)
--
-- instance Show SSAStatement where
--     show (SSAStatement index (Just reg) pin s (Just t)) = printf "      %d(%d): %s :%s\n" index reg (show s) (show t)
--     show (SSAStatement index Nothing pin s (Just t)) = printf "      %d: %s :%s\n" index (show s) (show t)
--     show (SSAStatement index (Just reg) pin s Nothing) = printf "      %d(%d): %s\n" index reg (show s)
--     show (SSAStatement index Nothing pin s Nothing) = printf "      %d: %s\n" index (show s)
--
-- instance Show Op where
--     show (Unify (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Unify %d %d" i1 i2
--     show (Alias (SSAStatement { ssaID = i1 })) = printf "Alias %d" i1
--     show This = printf "This"
--     show (Parameter i) = printf "Parameter *%d" i
--     show (Arg (SSAStatement { ssaID = i1 }) i2) = printf "Arg %d *%d" i1 i2
--     show (Null t) = printf "Null *Type(%s)" (show t)
--     show (SInt v) = printf "Int *%d" v
--     show (SBoolean v) = printf "Boolean *%s" (if v then "true" else "false")
--     show (NewObj name) = printf "NewObj *%s" name
--     show (NewIntArray (SSAStatement { ssaID = i })) = printf "NewIntArray *%d" i
--     show (Label label) = printf "Label *%s" label
--     show (Branch (SSAStatement { ssaID = i }) label) = printf "Branch %d *%s" i label
--     show (NBranch (SSAStatement { ssaID = i }) label) = printf "NBranch %d *%s" i label
--     show (Call (SSAStatement { ssaID = i }) (SSACall name args)) = printf "Call %d *%s(%s)" i name (intercalate ", " $ map (show . ssaID) args)
--     show (Print (SSAStatement { ssaID = i })) = printf "Print %d" i
--     show (Return (SSAStatement { ssaID = i })) = printf "Return %d" i
--     show (Member (SSAStatement { ssaID = i }) name) = printf "Member %d *%s" i name
--     show (Index (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Index %d %s" i1 i2
--     show (Store (SSAStatement { ssaID = i }) index) = printf "Store %d *%d" i index
--     show (Load index) = printf "Load *%d" index
--     show (VarAssg (SSAStatement { ssaID = i }) name) = printf "VarAssg %d *%s" i name
--     show (MemberAssg (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 }) name) = printf "MemberAssg %d %d *%s" i1 i2 name
--     show (IndexAssg (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 }) s) = printf "IndexAssg %d %d *%s" i1 i2 (show s)
--     show (Not (SSAStatement { ssaID = i1 })) = printf "Not %d" i1
--     show (Lt (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Lt %d %d" i1 i2
--     show (Le (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Le %d %d" i1 i2
--     show (Eq (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Eq %d %d" i1 i2
--     show (Ne (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Ne %d %d" i1 i2
--     show (Gt (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Gt %d %d" i1 i2
--     show (Ge (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Ge %d %d" i1 i2
--     show (And (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "And %d %d" i1 i2
--     show (Or (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Or %d %d" i1 i2
--     show (Plus (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Plus %d %d" i1 i2
--     show (Minus (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Minus %d %d" i1 i2
--     show (Mul (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Mul %d %d" i1 i2
--     show (Div (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Div %d %d" i1 i2
--     show (Mod (SSAStatement { ssaID = i1 }) (SSAStatement { ssaID = i2 })) = printf "Mod %d %d" i1 i2
--
-- instance Show StaticType where
--     show TypeInt = "Type(int)"
--     show TypeBoolean = "Type(boolean)"
--     show (TypeObject t) = show t
--
-- instance Show StaticTypeObject where
--     show (StaticTypeObject name _) = printf "Type(%s)" name

data SSAState info = SSAState { getProg :: AST.Program, getID :: Int, getBindings :: M.Map String (SSAStatement info) }

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
    return (ss ++ [Print r id])
scStatement (AST.ExpressionStatement e) = fst <$> scExp e

scExp :: AST.Exp -> State (SSAState Int) ([SSAStatement Int], SSAStatement Int)
scExp (AST.IntLiteral v) = SInt v <$> nextID >>= \r -> return ([r], r)
scExp (AST.BooleanLiteral v) = SBoolean v <$> nextID >>= \r -> return ([r], r)
scExp (AST.BinaryExpression l op r) = do
    (ssl, sl) <- scExp l
    (ssr, sr) <- scExp r
    id <- nextID
    return (case lookup op opConstructors of
        Just opConstructor -> let final = opConstructor sl sr id in (ssl ++ ssr ++ [final], final)
        Nothing -> error $ "Op " ++ op ++ " not found in list: " ++ show (map fst opConstructors))
scExp (AST.NotExp e) = do
    (ss, r) <- scExp e
    id <- nextID
    let final = Not r id
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
scParameter (ast, i) = SSAParameter ast <$> nextID <*> pure i

scVarDecl :: AST.VarDecl -> State (SSAState Int) (SSAStatement Int)
scVarDecl (AST.VarDecl t name) = do
    id <- nextID
    let r = Null id
    s@(SSAState { getBindings = bs }) <- get
    put (s { getBindings = M.insert name r bs })
    return r
