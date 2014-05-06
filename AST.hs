{-# LANGUAGE TemplateHaskell #-}

module AST where

import Control.Lens

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

makeLenses ''Variable
