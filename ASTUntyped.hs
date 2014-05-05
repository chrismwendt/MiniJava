{-# LANGUAGE TemplateHaskell #-}

module ASTUntyped where

import Text.Printf
import Data.List
import Control.Lens
import Data.Maybe

data Program = Program
    { _pMain :: Statement
    , _pClasses :: [Class]
    }
    deriving (Show, Eq)

data Class = Class
    { _cName :: String
    , _cParent :: Maybe String
    , _cVariables :: [Variable]
    , _cMethods :: [Method]
    }
    deriving (Show, Eq)

data Variable = Variable
    { _vType :: Type
    , _vName :: String
    }
    deriving (Show, Eq)

data Method = Method
    { _mReturnType :: Type
    , _mName :: String
    , _mParameters :: [Variable]
    , _mLocals :: [Variable]
    , _mStatements :: [Statement]
    , _mReturn :: Expression
    }
    deriving (Show, Eq)

data Statement =
      Block [Statement]
    | If Expression Statement (Maybe Statement)
    | While Expression Statement
    | Print Expression
    | ExpressionStatement Expression
    deriving (Show, Eq)

data Expression =
      LiteralInt Int
    | LiteralBoolean Bool
    | Assignment Expression Expression
    | Binary Expression BinaryOperator Expression
    | Not Expression
    | IndexGet Expression Expression
    | Call Expression String [Expression]
    | MemberGet Expression String
    | VariableGet String
    | This
    | NewIntArray Expression
    | NewObject String
    deriving (Show, Eq)

data BinaryOperator =
      Lt
    | Le
    | Eq
    | Ne
    | Gt
    | Ge
    | And
    | Or
    | Plus
    | Minus
    | Mul
    | Div
    | Mod
    deriving (Show, Eq)

data Type =
      TypeBoolean
    | TypeInt
    | TypeIntArray
    | TypeObject String
    deriving (Show, Eq)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Variable
makeLenses ''Method
makeLenses ''Type

-- TODO use Show instances instead of explicit string functions

soMany f as = concatMap (" " ++) $ map f as
sExpProgram (Program m cs) = printf "(%s (Main %s)%s)" "Program" (sExpStatement m) (soMany sExpClass cs) :: String
sExpClass (Class name (Just extends) vs ms) = printf "(%s %s %s%s%s)" "ClassDecl" (show name) (show extends) (soMany sExpVariable vs) (soMany sExpMethod ms) :: String
sExpClass (Class name Nothing vs ms) = printf "(%s %s %s%s%s)" "ClassDecl" (show name) "null" (soMany sExpVariable vs) (soMany sExpMethod ms) :: String
sExpVariable (Variable t name) = printf "(%s %s %s)" "VarDecl" (sExpType t) (show name) :: String
sExpMethod (Method t name ps vs ss ret) = printf "(%s %s %s (Parameters%s) (VarDecls%s) (Statements%s) (Return %s))" "MethodDecl" (sExpType t) (show name) (soMany sExpVariable ps) (soMany sExpVariable vs) (soMany sExpStatement ss) (sExp ret) :: String
sExpType TypeBoolean = printf "(%s)" "TypeBoolean" :: String
sExpType TypeInt = printf "(%s)" "TypeInt" :: String
sExpType TypeIntArray = printf "(%s)" "TypeIntArray" :: String
sExpType (TypeObject name) = printf "(Type %s)" (show name) :: String
sExpStatement (Block ss) = printf "(%s%s)" "BlockStatement" (soMany sExpStatement ss) :: String
sExpStatement (If e s (Just s2)) = printf "(%s %s %s %s)" "IfStatement" (sExp e) (sExpStatement s) (sExpStatement s2) :: String
sExpStatement (If e s Nothing) = printf "(%s %s %s)" "IfStatement" (sExp e) (sExpStatement s) :: String
sExpStatement (While e s) = printf "(%s %s %s)" "WhileStatement" (sExp e) (sExpStatement s) :: String
sExpStatement (Print e) = printf "(%s %s)" "PrintStatement" (sExp e) :: String
sExpStatement (ExpressionStatement e) = printf "(%s %s)" "ExpStatement" (sExp e) :: String
sExp (LiteralInt v) = printf "(%s %s)" "int" (show v) :: String
sExp (LiteralBoolean v) = printf "(%s %s)" "boolean" (if v then "true" else "false") :: String
sExp (Assignment e1 e2) = printf "(%s %s %s)" "AssignExp" (sExp e1) (sExp e2) :: String
sExp (Binary e1 op e2) = printf "(%s %s %s)" (fromJust $ lookup op ops) (sExp e1) (sExp e2) :: String
    where
    ops =
        [ (Lt, "<")
        , (Le, "<=")
        , (Eq, "==")
        , (Ne, "!=")
        , (Gt, ">")
        , (Ge, ">=")
        , (And, "&&")
        , (Or, "||")
        , (Plus, "+")
        , (Minus, "-")
        , (Mul, "*")
        , (Div, "/")
        , (Mod, "%")
        ]
sExp (Not e1) = printf "(%s %s)" "NotExp" (sExp e1) :: String
sExp (IndexGet e1 e2) = printf "(%s %s %s)" "IndexExp" (sExp e1) (sExp e2) :: String
sExp (Call e1 name args) = printf "(%s %s %s%s)" "CallExp" (sExp e1) (show name) (soMany sExp args) :: String
sExp (MemberGet e1 name) = printf "(%s %s %s)" "MemberGet" (sExp e1) (show name) :: String
sExp (VariableGet name) = printf "(%s %s)" "VarExp" (show name) :: String
sExp This = printf "(%s)" "ThisExp" :: String
sExp (NewIntArray e1) = printf "(%s %s)" "NewIntArray" (sExp e1) :: String
sExp (NewObject e1) = printf "(%s %s)" "new" (show e1) :: String
