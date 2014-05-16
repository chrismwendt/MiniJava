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

    | MemberGet  String Register String Register
    | MemberAssg String Register String Register Register

    | VarAssg Register Register

    | IndexGet    Register Register Register
    | IndexAssg   Register Register Register Register
    | ArrayLength Register Register

    | Not Register Register

    | Lt    Register Register Register
    | Le    Register Register Register
    | Eq    Register Register Register
    | Ne    Register Register Register
    | Gt    Register Register Register
    | Ge    Register Register Register
    | And   Register Register Register
    | Or    Register Register Register
    | Plus  Register Register Register
    | Minus Register Register Register
    | Mul   Register Register Register
    | Div   Register Register Register
    | Mod   Register Register Register
    deriving (Eq, Show)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Method
makeLenses ''Statement

def :: Statement -> Maybe Register
def (Load offset r)            = Just r
def (Null t r)                 = Just r
def (NewObj s1 r)              = Just r
def (NewIntArray r1 r)         = Just r
def (This r)                   = Just r
def (SInt v r)                 = Just r
def (SBoolean v r)             = Just r
def (Parameter position r)     = Just r
def (Call s1 r1 s2 is r)       = Just r
def (MemberGet s1 r1 s2 r)     = Just r
def (MemberAssg s1 r1 s2 r2 r) = Just r
def (VarAssg r1 r)             = Just r
def (IndexGet r1 r2 r)         = Just r
def (IndexAssg r1 r2 r3 r)     = Just r
def (ArrayLength r1 r)         = Just r
def (Not r1 r)                 = Just r
def (Lt r1 r2 r)               = Just r
def (Le r1 r2 r)               = Just r
def (Eq r1 r2 r)               = Just r
def (Ne r1 r2 r)               = Just r
def (Gt r1 r2 r)               = Just r
def (Ge r1 r2 r)               = Just r
def (And r1 r2 r)              = Just r
def (Or r1 r2 r)               = Just r
def (Plus r1 r2 r)             = Just r
def (Minus r1 r2 r)            = Just r
def (Mul r1 r2 r)              = Just r
def (Div r1 r2 r)              = Just r
def (Mod r1 r2 r)              = Just r
def (Store r1 offset)          = Nothing
def (Branch r1)                = Nothing
def (NBranch r1)               = Nothing
def (Arg r1 p)                 = Nothing
def (Return r1)                = Nothing
def (Print r1)                 = Nothing
def (BeginMethod)              = Nothing
def (Label)                    = Nothing
def (Goto)                     = Nothing
