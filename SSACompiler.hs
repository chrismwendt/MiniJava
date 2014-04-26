module SSACompiler where

import qualified AST
import Text.Printf
import Data.List

data SSAProgram = SSAProgram SSAStatement [SSAClass]

data SSAMethod = SSAMethod AST.MethodDecl [SSAStatement]

data SSAClass = SSAClass AST.ClassDecl [SSAField] [SSAMethod]

data SSAField = SSAField AST.VarDecl String Int StaticType

data SSACall = SSACall String [SSAStatement]

data SSAStatement = SSAStatement { ssaIndex :: Int, ssaReg :: Int, ssaPinned :: Bool, ssaOp :: Op }

data Op =
      Unify SSAStatement SSAStatement
    | Alias SSAStatement
    | This
    | Parameter Int
    | Arg SSAStatement Int
    | Null StaticType
    | SInt Int
    | SBoolean Bool
    | NewObj String
    | NewIntArray SSAStatement
    | Label String
    | Goto String
    | Branch SSAStatement String
    | NBranch SSAStatement String
    | Call SSAStatement SSACall
    | Print SSAStatement
    | Return SSAStatement
    | Member SSAStatement String
    | Index SSAStatement SSAStatement
    | Store SSAStatement Int
    | Load Int
    | VarAssg SSAStatement String
    | MemberAssg SSAStatement SSAStatement String
    | IndexAssg SSAStatement SSAStatement SSAStatement
    | Not SSAStatement
    | Lt SSAStatement SSAStatement
    | Le SSAStatement SSAStatement
    | Eq SSAStatement SSAStatement
    | Ne SSAStatement SSAStatement
    | Gt SSAStatement SSAStatement
    | Ge SSAStatement SSAStatement
    | And SSAStatement SSAStatement
    | Or SSAStatement SSAStatement
    | Plus SSAStatement SSAStatement
    | Minus SSAStatement SSAStatement
    | Mul SSAStatement SSAStatement
    | Div SSAStatement SSAStatement
    | Mod SSAStatement SSAStatement

data StaticType =
      TypeInt
    | TypeBoolean
    | TypeObject StaticObjectType

data StaticObjectType = StaticObjectType String (Maybe StaticObjectType)

instance Show SSAProgram where
    show (SSAProgram m cs) = printf "program:\n%s%s" (show m) (concatMap show cs)

instance Show SSAMethod where
    show (SSAMethod (AST.MethodDecl _ name _ _ _ _) ss) = printf "    method %s:\n%s" name (concatMap show ss)

instance Show SSAClass where
    show (SSAClass (AST.ClassDecl name _ _ _) fs ms) = printf "  class %s:\n%s" name (concatMap show ms)

instance Show SSAField where
    show (SSAField vd name index t) = name

instance Show SSACall where
    show (SSACall name ss) = printf "    method %s:\n%s" name (concatMap show ss)

instance Show SSAStatement where
    show (SSAStatement index reg pin s) = printf "      %d: %s\n" index (show s)

instance Show Op where
    show (Unify (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Unify %d %d" i1 i2
    show (Alias (SSAStatement { ssaIndex = i1 })) = printf "Alias %d" i1
    show This = printf "This"
    show (Parameter i) = printf "Parameter *%d" i
    show (Arg (SSAStatement { ssaIndex = i1 }) i2) = printf "Arg %d *%d" i1 i2
    show (Null t) = printf "Null *Type(%s)" (show t)
    show (SInt v) = printf "Int *%d" v
    show (SBoolean v) = printf "Boolean *%s" (if v then "true" else "false")
    show (NewObj name) = printf "NewObj *%s" name
    show (NewIntArray (SSAStatement { ssaIndex = i })) = printf "NewIntArray *%d" i
    show (Label label) = printf "Label *%s" label
    show (Branch (SSAStatement { ssaIndex = i }) label) = printf "Branch %d *%s" i label
    show (NBranch (SSAStatement { ssaIndex = i }) label) = printf "NBranch %d *%s" i label
    show (Call (SSAStatement { ssaIndex = i }) (SSACall name args)) = printf "Call %d *%s(%s)" i name (intercalate ", " $ map (show . ssaIndex) args)
    show (Print (SSAStatement { ssaIndex = i })) = printf "Print %d" i
    show (Return (SSAStatement { ssaIndex = i })) = printf "Return %d" i
    show (Member (SSAStatement { ssaIndex = i }) name) = printf "Member %d *%s" i name
    show (Index (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Index %d %s" i1 i2
    show (Store (SSAStatement { ssaIndex = i }) index) = printf "Store %d *%d" i index
    show (Load index) = printf "Load *%d" index
    show (VarAssg (SSAStatement { ssaIndex = i }) name) = printf "VarAssg %d *%s" i name
    show (MemberAssg (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 }) name) = printf "MemberAssg %d %d *%s" i1 i2 name
    show (IndexAssg (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 }) s) = printf "IndexAssg %d %d *%s" i1 i2 (show s)
    show (Not (SSAStatement { ssaIndex = i1 })) = printf "Not %d" i1
    show (Lt (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Lt %d %d" i1 i2
    show (Le (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Le %d %d" i1 i2
    show (Eq (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Eq %d %d" i1 i2
    show (Ne (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Ne %d %d" i1 i2
    show (Gt (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Gt %d %d" i1 i2
    show (Ge (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Ge %d %d" i1 i2
    show (And (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "And %d %d" i1 i2
    show (Or (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Or %d %d" i1 i2
    show (Plus (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Plus %d %d" i1 i2
    show (Minus (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Minus %d %d" i1 i2
    show (Mul (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Mul %d %d" i1 i2
    show (Div (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Div %d %d" i1 i2
    show (Mod (SSAStatement { ssaIndex = i1 }) (SSAStatement { ssaIndex = i2 })) = printf "Mod %d %d" i1 i2

instance Show StaticType where
    show TypeInt = "Type(int)"
    show TypeBoolean = "Type(boolean)"
    show (TypeObject t) = show t

instance Show StaticObjectType where
    show (StaticObjectType name _) = printf "Type(%s)" name

ssaCompileProgram _ = SSAProgram undefined undefined
