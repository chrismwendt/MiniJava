{-# LANGUAGE TemplateHaskell #-}

module SSA where

import qualified Data.Map as M
import Control.Lens
import Text.Printf
import Data.List
import Data.Graph.Inductive
import qualified AST as U

type ID = Int

type Position = Int

type Offset = Int

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
    , _mControlFlow :: Gr Statement EdgeType
    }
    deriving (Show)

data EdgeType = Step | Jump deriving (Show, Eq, Ord)

data Statement =
      BeginMethod

    | Unify ID ID

    | Store ID Offset
    | Load Offset

    | Null U.Type
    | NewObj String
    | NewIntArray ID
    | This
    | SInt Int
    | SBoolean Bool

    | Label
    | Goto
    | Branch ID
    | NBranch ID

    | Parameter Position
    | Arg ID Position
    | Call String ID String [ID]
    | Return ID

    | Print ID

    | MemberGet String ID String
    | MemberAssg String ID String ID

    | VarAssg ID

    | IndexGet ID ID
    | IndexAssg ID ID ID
    | ArrayLength ID

    | Not ID

    | Lt ID ID
    | Le ID ID
    | Eq ID ID
    | Ne ID ID
    | Gt ID ID
    | Ge ID ID
    | And ID ID
    | Or ID ID
    | Plus ID ID
    | Minus ID ID
    | Mul ID ID
    | Div ID ID
    | Mod ID ID
    deriving (Eq, Show)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Method
makeLenses ''Statement

mapIDs :: (ID -> ID) -> Statement -> Statement
mapIDs f (Load offset)            = Load offset
mapIDs f (Null t)                 = Null t
mapIDs f (NewObj s1)              = NewObj s1
mapIDs f (NewIntArray r1)         = NewIntArray (f r1)
mapIDs f (This)                   = This
mapIDs f (SInt v)                 = SInt v
mapIDs f (SBoolean v)             = SBoolean v
mapIDs f (Parameter position)     = Parameter position
mapIDs f (Call s1 r1 s2 is)       = Call s1 (f r1) s2 (map f is)
mapIDs f (MemberGet s1 r1 s2)     = MemberGet s1 (f r1) s2
mapIDs f (MemberAssg s1 r1 s2 r2) = MemberAssg s1 (f r1) s2 (f r2)
mapIDs f (VarAssg r1)             = VarAssg (f r1)
mapIDs f (IndexGet r1 r2)         = IndexGet (f r1) (f r2)
mapIDs f (IndexAssg r1 r2 r3)     = IndexAssg (f r1) (f r2) (f r3)
mapIDs f (ArrayLength r1)         = ArrayLength (f r1)
mapIDs f (Not r1)                 = Not (f r1)
mapIDs f (Lt r1 r2)               = Lt (f r1) (f r2)
mapIDs f (Le r1 r2)               = Le (f r1) (f r2)
mapIDs f (Eq r1 r2)               = Eq (f r1) (f r2)
mapIDs f (Ne r1 r2)               = Ne (f r1) (f r2)
mapIDs f (Gt r1 r2)               = Gt (f r1) (f r2)
mapIDs f (Ge r1 r2)               = Ge (f r1) (f r2)
mapIDs f (And r1 r2)              = And (f r1) (f r2)
mapIDs f (Or r1 r2)               = Or (f r1) (f r2)
mapIDs f (Plus r1 r2)             = Plus (f r1) (f r2)
mapIDs f (Minus r1 r2)            = Minus (f r1) (f r2)
mapIDs f (Mul r1 r2)              = Mul (f r1) (f r2)
mapIDs f (Div r1 r2)              = Div (f r1) (f r2)
mapIDs f (Mod r1 r2)              = Mod (f r1) (f r2)
mapIDs f (Store r1 offset)        = Store (f r1) offset
mapIDs f (Branch r1)              = Branch (f r1)
mapIDs f (NBranch r1)             = NBranch (f r1)
mapIDs f (Arg r1 p)               = Arg (f r1) p
mapIDs f (Return r1)              = Return (f r1)
mapIDs f (Print r1)               = Print (f r1)
mapIDs f (Unify i1 i2)            = Unify (f i1) (f i2)
mapIDs _ (BeginMethod)            = BeginMethod
mapIDs _ (Label)                  = Label
mapIDs _ (Goto)                   = Goto
