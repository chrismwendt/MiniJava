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
