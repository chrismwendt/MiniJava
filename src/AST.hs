{-# LANGUAGE TemplateHaskell #-}

module AST where

import Control.Lens

data Program = Program
  { _pMain    :: Class
  , _pClasses :: [Class]
  }
  deriving (Show, Eq)

data Class = Class
  { _cName    :: String
  , _cParent  :: String
  , _cFields  :: [Variable]
  , _cMethods :: [Method]
  }
  deriving (Show, Eq)

data Method = Method
  { _mReturnType :: Type
  , _mName       :: String
  , _mParameters :: [Variable]
  , _mLocals     :: [Variable]
  , _mStatements :: [Statement]
  , _mReturn     :: Expression
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
  | Binary Expression AST.BinaryOperator Expression
  | Not Expression
  | IndexGet Expression Expression
  | Call Expression String [Expression]
  | MemberGet Expression String
  | VariableGet String
  | This
  | NewIntArray Expression
  | NewObject String
  deriving (Show, Eq)

data Variable = Variable
  { _vType :: Type
  , _vName :: String
  }
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
    TypeVoid
  | TypeBoolean
  | TypeInt
  | TypeIntArray
  | TypeObject String
  deriving (Show, Eq)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Variable
makeLenses ''Method
