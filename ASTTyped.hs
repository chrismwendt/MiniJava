{-# LANGUAGE TemplateHaskell #-}

module ASTTyped where

import Text.Printf
import Data.List
import Control.Lens
import Data.Maybe
import AST

data Program = Program
    { _pMain :: Statement
    , _pClasses :: [Class]
    }
    deriving (Show, Eq)

data Class = Class
    { _cName :: String
    , _cParent :: String
    , _cFields :: [Variable]
    , _cMethods :: [Method]
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
    | Binary Expression AST.BinaryOperator Expression
    | Not Expression
    | Call String Expression String [Expression]
    | MemberGet String Expression String
    | MemberAssignment String Expression String Expression
    | VariableGet String
    | VariableAssignment String Expression
    | IndexGet Expression Expression
    | IndexAssignment Expression Expression Expression
    | IntArrayLength Expression
    | This
    | NewIntArray Expression
    | NewObject String
    deriving (Show, Eq)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Variable
makeLenses ''Method
