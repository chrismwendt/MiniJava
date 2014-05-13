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
import qualified Data.Set as Set

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

def :: Statement -> Set.Set Register
def (Load offset r)            = Set.fromList [r]
def (Null t r)                 = Set.fromList [r]
def (NewObj s1 r)              = Set.fromList [r]
def (NewIntArray r1 r)         = Set.fromList [r]
def (This r)                   = Set.fromList [r]
def (SInt v r)                 = Set.fromList [r]
def (SBoolean v r)             = Set.fromList [r]
def (Parameter position r)     = Set.fromList [r]
def (Call s1 r1 s2 is r)       = Set.fromList [r]
def (MemberGet s1 r1 s2 r)     = Set.fromList [r]
def (MemberAssg s1 r1 s2 r2 r) = Set.fromList [r]
def (VarAssg r1 r)             = Set.fromList [r]
def (IndexGet r1 r2 r)         = Set.fromList [r]
def (IndexAssg r1 r2 r3 r)     = Set.fromList [r]
def (Not r1 r)                 = Set.fromList [r]
def (Lt r1 r2 r)               = Set.fromList [r]
def (Le r1 r2 r)               = Set.fromList [r]
def (Eq r1 r2 r)               = Set.fromList [r]
def (Ne r1 r2 r)               = Set.fromList [r]
def (Gt r1 r2 r)               = Set.fromList [r]
def (Ge r1 r2 r)               = Set.fromList [r]
def (And r1 r2 r)              = Set.fromList [r]
def (Or r1 r2 r)               = Set.fromList [r]
def (Plus r1 r2 r)             = Set.fromList [r]
def (Minus r1 r2 r)            = Set.fromList [r]
def (Mul r1 r2 r)              = Set.fromList [r]
def (Div r1 r2 r)              = Set.fromList [r]
def (Mod r1 r2 r)              = Set.fromList [r]
def (Store r1 offset)          = Set.fromList []
def (Branch r1)                = Set.fromList []
def (NBranch r1)               = Set.fromList []
def (Arg r1 p)                 = Set.fromList []
def (Return r1)                = Set.fromList []
def (Print r1)                 = Set.fromList []
def (BeginMethod)              = Set.fromList []
def (Label)                    = Set.fromList []
def (Goto)                     = Set.fromList []
