{-# LANGUAGE TemplateHaskell #-}

module SSARegisters where

import qualified AST as AST
import qualified ASTTyped as T
import qualified Data.Map as M
import Control.Lens
import Text.Printf
import Data.List

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
    , _mStatements :: [ID]
    , _mIDToS :: M.Map ID Statement
    }
    deriving (Show)

data Statement =
      Store ID Offset
    | Load Offset Register

    | Null AST.Type Register
    | NewObj String Register
    | NewIntArray ID Register
    | This Register
    | SInt Int Register
    | SBoolean Bool Register

    | Label String
    | Goto String
    | Branch ID String
    | NBranch ID String

    | Parameter ID Register
    | Arg ID Position
    | Call String ID String [ID] Register
    | Return ID

    | Print ID

    | MemberGet String ID String Register
    | MemberAssg String ID String ID Register

    | VarAssg ID Register

    | IndexGet ID ID Register
    | IndexAssg ID ID ID Register

    | Not ID Register

    | Lt ID ID Register
    | Le ID ID Register
    | Eq ID ID Register
    | Ne ID ID Register
    | Gt ID ID Register
    | Ge ID ID Register
    | And ID ID Register
    | Or ID ID Register
    | Plus ID ID Register
    | Minus ID ID Register
    | Mul ID ID Register
    | Div ID ID Register
    | Mod ID ID Register
    deriving (Eq, Show)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Method
makeLenses ''Statement
