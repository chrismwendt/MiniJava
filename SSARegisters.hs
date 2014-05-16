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
def (Load _ r)             = Just r
def (Null _ r)             = Just r
def (NewObj _ r)           = Just r
def (NewIntArray _ r)      = Just r
def (This r)               = Just r
def (SInt _ r)             = Just r
def (SBoolean _ r)         = Just r
def (Parameter _ r)        = Just r
def (Call _ _ _ _ r)       = Just r
def (MemberGet _ _ _ r)    = Just r
def (MemberAssg _ _ _ _ r) = Just r
def (VarAssg _ r)          = Just r
def (IndexGet _ _ r)       = Just r
def (IndexAssg _ _ _ r)    = Just r
def (ArrayLength _ r)      = Just r
def (Not _ r)              = Just r
def (Lt _ _ r)             = Just r
def (Le _ _ r)             = Just r
def (Eq _ _ r)             = Just r
def (Ne _ _ r)             = Just r
def (Gt _ _ r)             = Just r
def (Ge _ _ r)             = Just r
def (And _ _ r)            = Just r
def (Or _ _ r)             = Just r
def (Plus _ _ r)           = Just r
def (Minus _ _ r)          = Just r
def (Mul _ _ r)            = Just r
def (Div _ _ r)            = Just r
def (Mod _ _ r)            = Just r
def (Store _ _)            = Nothing
def (Branch _)             = Nothing
def (NBranch _)            = Nothing
def (Arg _ _)              = Nothing
def (Return _)             = Nothing
def (Print _)              = Nothing
def (BeginMethod)          = Nothing
def (Label)                = Nothing
def (Goto)                 = Nothing

uses :: Statement -> Set.Set Register
uses (Load offset r)            = Set.fromList []
uses (Null t r)                 = Set.fromList []
uses (NewObj s1 r)              = Set.fromList []
uses (NewIntArray r1 r)         = Set.fromList [r1]
uses (This r)                   = Set.fromList []
uses (SInt v r)                 = Set.fromList []
uses (SBoolean v r)             = Set.fromList []
uses (Parameter position r)     = Set.fromList []
uses (Call s1 r1 s2 is r)       = Set.fromList [r1]
uses (MemberGet s1 r1 s2 r)     = Set.fromList [r1]
uses (MemberAssg s1 r1 s2 r2 r) = Set.fromList [r1, r2]
uses (VarAssg r1 r)             = Set.fromList [r1]
uses (IndexGet r1 r2 r)         = Set.fromList [r1, r2]
uses (IndexAssg r1 r2 r3 r)     = Set.fromList [r1, r2, r3]
uses (ArrayLength r1 r)         = Set.fromList [r1]
uses (Not r1 r)                 = Set.fromList [r1]
uses (Lt r1 r2 r)               = Set.fromList [r1, r2]
uses (Le r1 r2 r)               = Set.fromList [r1, r2]
uses (Eq r1 r2 r)               = Set.fromList [r1, r2]
uses (Ne r1 r2 r)               = Set.fromList [r1, r2]
uses (Gt r1 r2 r)               = Set.fromList [r1, r2]
uses (Ge r1 r2 r)               = Set.fromList [r1, r2]
uses (And r1 r2 r)              = Set.fromList [r1, r2]
uses (Or r1 r2 r)               = Set.fromList [r1, r2]
uses (Plus r1 r2 r)             = Set.fromList [r1, r2]
uses (Minus r1 r2 r)            = Set.fromList [r1, r2]
uses (Mul r1 r2 r)              = Set.fromList [r1, r2]
uses (Div r1 r2 r)              = Set.fromList [r1, r2]
uses (Mod r1 r2 r)              = Set.fromList [r1, r2]
uses (Store r1 offset)          = Set.fromList [r1]
uses (Branch r1)                = Set.fromList [r1]
uses (NBranch r1)               = Set.fromList [r1]
uses (Arg r1 p)                 = Set.fromList [r1]
uses (Return r1)                = Set.fromList [r1]
uses (Print r1)                 = Set.fromList [r1]
uses (BeginMethod)              = Set.fromList []
uses (Label)                    = Set.fromList []
uses (Goto)                     = Set.fromList []

mapRegs f (Load offset r)            = Load offset (f r)
mapRegs f (Null t r)                 = Null t (f r)
mapRegs f (NewObj s1 r)              = NewObj s1 (f r)
mapRegs f (NewIntArray r1 r)         = NewIntArray (f r1) (f r)
mapRegs f (This r)                   = This (f r)
mapRegs f (SInt v r)                 = SInt v (f r)
mapRegs f (SBoolean v r)             = SBoolean v (f r)
mapRegs f (Parameter position r)     = Parameter position (f r)
mapRegs f (Call s1 r1 s2 is r)       = Call s1 (f r1) s2 is (f r)
mapRegs f (MemberGet s1 r1 s2 r)     = MemberGet s1 (f r1) s2 (f r)
mapRegs f (MemberAssg s1 r1 s2 r2 r) = MemberAssg s1 (f r1) s2 (f r2) (f r)
mapRegs f (VarAssg r1 r)             = VarAssg (f r1) (f r)
mapRegs f (IndexGet r1 r2 r)         = IndexGet (f r1) (f r2) (f r)
mapRegs f (IndexAssg r1 r2 r3 r)     = IndexAssg (f r1) (f r2) (f r3) (f r)
mapRegs f (ArrayLength r1 r)         = ArrayLength (f r1) (f r)
mapRegs f (Not r1 r)                 = Not (f r1) (f r)
mapRegs f (Lt r1 r2 r)               = Lt (f r1) (f r2) (f r)
mapRegs f (Le r1 r2 r)               = Le (f r1) (f r2) (f r)
mapRegs f (Eq r1 r2 r)               = Eq (f r1) (f r2) (f r)
mapRegs f (Ne r1 r2 r)               = Ne (f r1) (f r2) (f r)
mapRegs f (Gt r1 r2 r)               = Gt (f r1) (f r2) (f r)
mapRegs f (Ge r1 r2 r)               = Ge (f r1) (f r2) (f r)
mapRegs f (And r1 r2 r)              = And (f r1) (f r2) (f r)
mapRegs f (Or r1 r2 r)               = Or (f r1) (f r2) (f r)
mapRegs f (Plus r1 r2 r)             = Plus (f r1) (f r2) (f r)
mapRegs f (Minus r1 r2 r)            = Minus (f r1) (f r2) (f r)
mapRegs f (Mul r1 r2 r)              = Mul (f r1) (f r2) (f r)
mapRegs f (Div r1 r2 r)              = Div (f r1) (f r2) (f r)
mapRegs f (Mod r1 r2 r)              = Mod (f r1) (f r2) (f r)
mapRegs f (Store r1 offset)          = Store (f r1) offset
mapRegs f (Branch r1)                = Branch (f r1)
mapRegs f (NBranch r1)               = NBranch (f r1)
mapRegs f (Arg r1 p)                 = Arg (f r1) p
mapRegs f (Return r1)                = Return (f r1)
mapRegs f (Print r1)                 = Print (f r1)
mapRegs f (BeginMethod)              = BeginMethod
mapRegs f (Label)                    = Label
mapRegs f (Goto)                     = Goto
