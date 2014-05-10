{-# LANGUAGE TemplateHaskell #-}

module SSARegisters where

import qualified AST as AST
import qualified ASTTyped as T
import qualified Data.Map as M
import Control.Lens
import Text.Printf
import Data.List
import Data.Graph.Inductive
import qualified SSA as S

type ID = Int

type Position = Int

type Offset = Int

type Register = Int

data Program = Program
    { _pMain :: Class
    , _pClasses :: [Class]
    }
    deriving (Show)

data Class = Class
    { _cName :: String
    , _cFields :: [String]
    , _cMethods :: [Method]
    }
    deriving (Show)

data Method = Method
    { _mName :: String
    , _mControlFlow :: Gr Statement S.EdgeType
    }
    deriving (Show)

data Statement =
      BeginMethod

    | Store Register Offset
    | Load Offset Register

    | Null AST.Type Register
    | NewObj String Register
    | NewIntArray Register Register
    | This Register
    | SInt Int Register
    | SBoolean Bool Register

    | Label
    | Goto
    | Branch Register
    | NBranch Register

    | Parameter Position Register
    | Arg Register Position
    | Call String Register String [Register] Register
    | Return Register

    | Print Register

    | MemberGet String Register String Register
    | MemberAssg String Register String Register Register

    | VarAssg Register Register

    | IndexGet Register Register Register
    | IndexAssg Register Register Register Register
    | ArrayLength Register

    | Not Register Register

    | Lt Register Register Register
    | Le Register Register Register
    | Eq Register Register Register
    | Ne Register Register Register
    | Gt Register Register Register
    | Ge Register Register Register
    | And Register Register Register
    | Or Register Register Register
    | Plus Register Register Register
    | Minus Register Register Register
    | Mul Register Register Register
    | Div Register Register Register
    | Mod Register Register Register
    deriving (Eq, Show)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Method
makeLenses ''Statement
