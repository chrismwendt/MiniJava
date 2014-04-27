module SSACompiler where

import qualified AST
import Text.Printf
import Data.List
import Control.Monad.Reader
import Data.Functor
import Control.Applicative

data SSAProgram info = SSAProgram AST.Program [SSAStatement info] [SSAClass info] deriving (Show)

data SSAClass info = SSAClass AST.ClassDecl [SSAField info] [SSAMethod info] deriving (Show)

data SSAField info = SSAField AST.VarDecl Int info deriving (Show)

data SSAMethod info = SSAMethod AST.MethodDecl [SSAParameter info] [SSAStatement info] (SSAReturn info) deriving (Show)

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

-- instance Show SSAProgram where
--     show (SSAProgram m cs) = printf "program:\n%s%s" (show m) (concatMap show cs)
--
-- instance Show SSAMethod where
--     show (SSAMethod (AST.MethodDecl _ name _ _ _ _) ss) = printf "    method %s:\n%s" name (concatMap show ss)
--
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

ssaCompileProgram :: Reader AST.Program (SSAProgram ())
ssaCompileProgram = do
    ast@(AST.Program s cs) <- ask
    SSAProgram ast <$> ssaCompileStatement s <*> (mapM ssaCompileClassDecl cs)

ssaCompileStatement :: AST.Statement -> Reader AST.Program [SSAStatement ()]
ssaCompileStatement s = return [] --concatMap (ssaCompileStatement ast) ss

ssaCompileClassDecl :: AST.ClassDecl -> Reader AST.Program (SSAClass ())
ssaCompileClassDecl c = return $ SSAClass c [] []
