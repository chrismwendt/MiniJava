{-# LANGUAGE TemplateHaskell #-}

module AST where

import Text.Printf
import Data.List
import Control.Lens

data Program = Program 
    { _pMain :: Statement
    , _pClassDecls :: [ClassDecl]
    }
    deriving (Show, Eq)

data ClassDecl = ClassDecl 
    { _cName :: String
    , _cParent :: Maybe String
    , _cVarDecls :: [VarDecl]
    , _cMethodDecls :: [MethodDecl]
    }
    deriving (Show, Eq)

data VarDecl = VarDecl
    { _vType :: Type
    , _vName :: String
    }
    deriving (Show, Eq)

data MethodDecl = MethodDecl
    { _mReturnType :: Type
    , _mName :: String
    , _mParameters :: [Parameter]
    , _mVarDecls :: [VarDecl]
    , _mStatements :: [Statement]
    , _mReturnExp :: Exp
    }
    deriving (Show, Eq)

data Parameter = Parameter
    { _parType :: Type
    , _parName :: String
    }
    deriving (Show, Eq)

data Statement =
      BlockStatement [Statement]
    | IfStatement Exp Statement (Maybe Statement)
    | WhileStatement Exp Statement
    | PrintStatement Exp
    | ExpressionStatement Exp
    deriving (Show, Eq)

data Exp =
      IntLiteral Int
    | BooleanLiteral Bool
    | AssignExpression Exp Exp
    | BinaryExpression Exp String Exp
    | NotExp Exp
    | IndexExp Exp Exp
    | CallExp Exp String [Exp]
    | MemberExp Exp String
    | VarExp String
    | ThisExp
    | NewIntArrayExp Exp
    | NewObjectExp String
    deriving (Show, Eq)

data Type =
      BooleanType
    | IntType
    | IntArrayType
    | ObjectType String
    deriving (Show, Eq)

makeLenses ''Program
makeLenses ''ClassDecl
makeLenses ''VarDecl
makeLenses ''MethodDecl
makeLenses ''Parameter
makeLenses ''Type

-- TODO use Show instances instead of explicit string functions

soMany f as = concatMap (" " ++) $ map f as
sExpProgram (Program m cs) = printf "(%s (Main %s)%s)" "Program" (sExpStatement m) (soMany sExpClass cs) :: String
sExpClass (ClassDecl name (Just extends) vs ms) = printf "(%s %s %s%s%s)" "ClassDecl" (show name) (show extends) (soMany sExpVarDeclaration vs) (soMany sExpMethodDeclaration ms) :: String
sExpClass (ClassDecl name Nothing vs ms) = printf "(%s %s %s%s%s)" "ClassDecl" (show name) "null" (soMany sExpVarDeclaration vs) (soMany sExpMethodDeclaration ms) :: String
sExpVarDeclaration (VarDecl t name) = printf "(%s %s %s)" "VarDecl" (sExpType t) (show name) :: String
sExpMethodDeclaration (MethodDecl t name ps vs ss ret) = printf "(%s %s %s (Parameters%s) (VarDecls%s) (Statements%s) (Return %s))" "MethodDecl" (sExpType t) (show name) (soMany sExpParameter ps) (soMany sExpVarDeclaration vs) (soMany sExpStatement ss) (sExpExpression ret) :: String
sExpParameter (Parameter t name) = printf "(%s %s %s)" "Parameter" (sExpType t) (show name) :: String
sExpType BooleanType = printf "(%s)" "TypeBoolean" :: String
sExpType IntType = printf "(%s)" "TypeInt" :: String
sExpType IntArrayType = printf "(%s)" "TypeIntArray" :: String
sExpType (ObjectType name) = printf "(Type %s)" (show name) :: String
sExpStatement (BlockStatement ss) = printf "(%s%s)" "BlockStatement" (soMany sExpStatement ss) :: String
sExpStatement (IfStatement e s (Just s2)) = printf "(%s %s %s %s)" "IfStatement" (sExpExpression e) (sExpStatement s) (sExpStatement s2) :: String
sExpStatement (IfStatement e s Nothing) = printf "(%s %s %s)" "IfStatement" (sExpExpression e) (sExpStatement s) :: String
sExpStatement (WhileStatement e s) = printf "(%s %s %s)" "WhileStatement" (sExpExpression e) (sExpStatement s) :: String
sExpStatement (PrintStatement e) = printf "(%s %s)" "PrintStatement" (sExpExpression e) :: String
sExpStatement (ExpressionStatement e) = printf "(%s %s)" "ExpStatement" (sExpExpression e) :: String
sExpExpression (IntLiteral v) = printf "(%s %s)" "int" (show v) :: String
sExpExpression (BooleanLiteral v) = printf "(%s %s)" "boolean" (if v then "true" else "false") :: String
sExpExpression (AssignExpression e1 e2) = printf "(%s %s %s)" "AssignExp" (sExpExpression e1) (sExpExpression e2) :: String
sExpExpression (BinaryExpression e1 op e2) = printf "(%s %s %s)" op (sExpExpression e1) (sExpExpression e2) :: String
sExpExpression (NotExp e1) = printf "(%s %s)" "NotExp" (sExpExpression e1) :: String
sExpExpression (IndexExp e1 e2) = printf "(%s %s %s)" "IndexExp" (sExpExpression e1) (sExpExpression e2) :: String
sExpExpression (CallExp e1 name args) = printf "(%s %s %s%s)" "CallExp" (sExpExpression e1) (show name) (soMany sExpExpression args) :: String
sExpExpression (MemberExp e1 name) = printf "(%s %s %s)" "MemberExp" (sExpExpression e1) (show name) :: String
sExpExpression (VarExp name) = printf "(%s %s)" "VarExp" (show name) :: String
sExpExpression ThisExp = printf "(%s)" "ThisExp" :: String
sExpExpression (NewIntArrayExp e1) = printf "(%s %s)" "NewIntArrayExp" (sExpExpression e1) :: String
sExpExpression (NewObjectExp e1) = printf "(%s %s)" "new" (show e1) :: String
