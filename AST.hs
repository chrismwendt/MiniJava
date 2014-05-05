module AST where

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
