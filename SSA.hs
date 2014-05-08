{-# LANGUAGE TemplateHaskell #-}

module SSA where

import qualified AST as AST
import qualified ASTTyped as T
import qualified Data.Map as M
import Control.Lens
import Text.Printf
import Data.List

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

-- TODO consider storing statements in a graph
-- EdgeLabel = Step | Jump
-- flatten the graph into a list later

data Method = Method
    { _mName :: String
    , _mStatements :: [ID]
    , _mIDToS :: M.Map ID Statement
    }
    deriving (Show)

data Statement =
      Unify ID ID

    | Store ID Offset
    | Load Offset

    | Null AST.Type
    | NewObj String
    | NewIntArray ID
    | This
    | SInt Int
    | SBoolean Bool

    | Label String
    | Goto String
    | Branch ID String
    | NBranch ID String

    | Parameter ID
    | Arg ID Position
    | Call String ID String [ID]
    | Return ID

    | Print ID

    | MemberGet String ID String
    | MemberAssg String ID String ID

    | VarAssg ID

    | IndexGet ID ID
    | IndexAssg ID ID ID

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
