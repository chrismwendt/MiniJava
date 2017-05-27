{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

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

type Statement = StatementA Register

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

data StatementA a =
    BeginMethod

  | Store a Offset
  | Load Offset a

  | Null AST.Type a
  | NewObj String a
  | NewIntArray a a
  | This a
  | SInt Int a
  | SBoolean Bool a

  | Label
  | Goto
  | Branch a
  | NBranch a

  | Parameter Position a
  | Arg a Position
  | Call String a String a
  | Return a

  | Print a

  | MemberGet  String a String a
  | MemberAssg String a String a a

  | VarAssg a a

  | IndexGet    a a a
  | IndexAssg   a a a a
  | ArrayLength a a

  | Not a a

  | Lt    a a a
  | Le    a a a
  | Eq    a a a
  | Ne    a a a
  | Gt    a a a
  | Ge    a a a
  | And   a a a
  | Or    a a a
  | Plus  a a a
  | Minus a a a
  | Mul   a a a
  | Div   a a a
  | Mod   a a a
  deriving (Eq, Show, Functor)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Method
makeLenses ''StatementA

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
