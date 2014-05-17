{-# LANGUAGE TemplateHaskell #-}

module SSARegisters where

import Control.Lens
import Data.Graph.Inductive
import qualified Data.Set as Set
import qualified AST as AST
import qualified SSA as SSA

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
    , _mControlFlow :: Gr Statement SSA.EdgeType
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
    | Call String Register String Register
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
def (Call _ _ _ r)         = Just r
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
uses (Load _ _)               = Set.fromList []
uses (Null _ _)               = Set.fromList []
uses (NewObj _ _)             = Set.fromList []
uses (NewIntArray r1 _)       = Set.fromList [r1]
uses (This _)                 = Set.fromList []
uses (SInt _ _)               = Set.fromList []
uses (SBoolean _ _)           = Set.fromList []
uses (Parameter _ _)          = Set.fromList []
uses (Call _ r1 _ _)          = Set.fromList [r1]
uses (MemberGet _ r1 _ _)     = Set.fromList [r1]
uses (MemberAssg _ r1 _ r2 _) = Set.fromList [r1, r2]
uses (VarAssg r1 _)           = Set.fromList [r1]
uses (IndexGet r1 r2 _)       = Set.fromList [r1, r2]
uses (IndexAssg r1 r2 r3 _)   = Set.fromList [r1, r2, r3]
uses (ArrayLength r1 _)       = Set.fromList [r1]
uses (Not r1 _)               = Set.fromList [r1]
uses (Lt r1 r2 _)             = Set.fromList [r1, r2]
uses (Le r1 r2 _)             = Set.fromList [r1, r2]
uses (Eq r1 r2 _)             = Set.fromList [r1, r2]
uses (Ne r1 r2 _)             = Set.fromList [r1, r2]
uses (Gt r1 r2 _)             = Set.fromList [r1, r2]
uses (Ge r1 r2 _)             = Set.fromList [r1, r2]
uses (And r1 r2 _)            = Set.fromList [r1, r2]
uses (Or r1 r2 _)             = Set.fromList [r1, r2]
uses (Plus r1 r2 _)           = Set.fromList [r1, r2]
uses (Minus r1 r2 _)          = Set.fromList [r1, r2]
uses (Mul r1 r2 _)            = Set.fromList [r1, r2]
uses (Div r1 r2 _)            = Set.fromList [r1, r2]
uses (Mod r1 r2 _)            = Set.fromList [r1, r2]
uses (Store r1 _)             = Set.fromList [r1]
uses (Branch r1)              = Set.fromList [r1]
uses (NBranch r1)             = Set.fromList [r1]
uses (Arg r1 _)               = Set.fromList [r1]
uses (Return r1)              = Set.fromList [r1]
uses (Print r1)               = Set.fromList [r1]
uses (BeginMethod)            = Set.fromList []
uses (Label)                  = Set.fromList []
uses (Goto)                   = Set.fromList []

mapRegs :: (Register -> Register) -> Statement -> Statement
mapRegs f (Load offset r)            = Load offset (f r)
mapRegs f (Null t r)                 = Null t (f r)
mapRegs f (NewObj s1 r)              = NewObj s1 (f r)
mapRegs f (NewIntArray r1 r)         = NewIntArray (f r1) (f r)
mapRegs f (This r)                   = This (f r)
mapRegs f (SInt v r)                 = SInt v (f r)
mapRegs f (SBoolean v r)             = SBoolean v (f r)
mapRegs f (Parameter position r)     = Parameter position (f r)
mapRegs f (Call s1 r1 s2 r)          = Call s1 (f r1) s2 (f r)
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
mapRegs _ (BeginMethod)              = BeginMethod
mapRegs _ (Label)                    = Label
mapRegs _ (Goto)                     = Goto

withRegister :: SSA.Statement -> Either (SSA.ID -> Statement) Statement
withRegister (SSA.Load offset)            = Left  $ Load offset
withRegister (SSA.Null t)                 = Left  $ Null t
withRegister (SSA.NewObj s1)              = Left  $ NewObj s1
withRegister (SSA.NewIntArray i1)         = Left  $ NewIntArray i1
withRegister (SSA.This)                   = Left  $ This
withRegister (SSA.SInt v)                 = Left  $ SInt v
withRegister (SSA.SBoolean v)             = Left  $ SBoolean v
withRegister (SSA.Parameter position)     = Left  $ Parameter position
withRegister (SSA.Call s1 i1 s2 is)       = Left  $ Call s1 i1 s2
withRegister (SSA.MemberGet s1 i1 s2)     = Left  $ MemberGet s1 i1 s2
withRegister (SSA.MemberAssg s1 i1 s2 i2) = Left  $ MemberAssg s1 i1 s2 i2
withRegister (SSA.VarAssg i1)             = Left  $ VarAssg i1
withRegister (SSA.IndexGet i1 i2)         = Left  $ IndexGet i1 i2
withRegister (SSA.IndexAssg i1 i2 i3)     = Left  $ IndexAssg i1 i2 i3
withRegister (SSA.ArrayLength i1)         = Left  $ ArrayLength i1
withRegister (SSA.Not i1)                 = Left  $ Not i1
withRegister (SSA.Lt i1 i2)               = Left  $ Lt i1 i2
withRegister (SSA.Le i1 i2)               = Left  $ Le i1 i2
withRegister (SSA.Eq i1 i2)               = Left  $ Eq i1 i2
withRegister (SSA.Ne i1 i2)               = Left  $ Ne i1 i2
withRegister (SSA.Gt i1 i2)               = Left  $ Gt i1 i2
withRegister (SSA.Ge i1 i2)               = Left  $ Ge i1 i2
withRegister (SSA.And i1 i2)              = Left  $ And i1 i2
withRegister (SSA.Or i1 i2)               = Left  $ Or i1 i2
withRegister (SSA.Plus i1 i2)             = Left  $ Plus i1 i2
withRegister (SSA.Minus i1 i2)            = Left  $ Minus i1 i2
withRegister (SSA.Mul i1 i2)              = Left  $ Mul i1 i2
withRegister (SSA.Div i1 i2)              = Left  $ Div i1 i2
withRegister (SSA.Mod i1 i2)              = Left  $ Mod i1 i2
withRegister (SSA.Store i1 offset)        = Right $ Store i1 offset
withRegister (SSA.Branch i1)              = Right $ Branch i1
withRegister (SSA.NBranch i1)             = Right $ NBranch i1
withRegister (SSA.Arg i1 p)               = Right $ Arg i1 p
withRegister (SSA.Return i1)              = Right $ Return i1
withRegister (SSA.Print i1)               = Right $ Print i1
withRegister (SSA.BeginMethod)            = Right $ BeginMethod
withRegister (SSA.Label)                  = Right $ Label
withRegister (SSA.Goto)                   = Right $ Goto
withRegister (SSA.Unify _ _)              = error "withRegister of Unify"
