{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module SSA where

import qualified AST as AST
import qualified ASTTyped as T
import qualified Data.Map as M
import Control.Lens
import Text.Printf
import Data.List

type ID = Int

data Program = Program
    { _pProgram :: T.Program
    , _pMain :: [ID]
    , _pClasses :: [Class]
    }

data Class = Class
    { _cClass :: T.Class
    , _cFields :: [Field]
    , _cMethod :: [Method]
    }

data Field = Field
    { _fVariable :: AST.Variable
    , _fPosition :: Int
    }

data Method = Method
    { _mMethod :: T.Method
    , _mParameters :: [ID]
    , _mStatements :: [ID]
    , _mReturn :: ID
    }

data Statement =
      Unify ID ID
    | Alias ID
    | This
    | Variable String Int
    | Arg ID Int
    | Null AST.Type
    | SInt Int
    | SBoolean Bool
    | NewObj String
    | NewIntArray ID
    | Label String
    | Goto String
    | Branch ID String
    | NBranch ID String
    | Call String ID String [ID]
    | Print ID
    | Return ID
    | MemberGet String ID String
    | IndexGet ID ID
    | Store ID Int
    | Load Int
    | VarAssg String ID
    | MemberAssg String ID String ID
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
    deriving (Eq)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Field
makeLenses ''Method
makeLenses ''Statement

instance Show Program where
    show (Program _ ss cs) = printf "program:\n  main:\n    method main:\n%s%s" (concatMap show ss) (concatMap show cs)

instance Show Method where
    show (Method (T.Method _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s" name (unlines $ map show ss)

instance Show Class where
    show (Class (T.Class name _ _ _) _ ms) = printf "  class %s:\n%s" name (concatMap show ms)

instance Show Field where
    show (Field (AST.Variable _ name) _) = name

instance Show Statement where
    show (Unify l r) = printf "Unify %s %s" (show l) (show r)
    show (Alias s) = printf "Alias %s" (show s)
    show This = printf "This"
    show (Variable _ index) = printf "Variable *%s" (show index)
    show (Arg arg index) = printf "Arg %s *%s" (show arg) (show index)
    show (Null AST.TypeBoolean) = printf "Null *Type(boolean)"
    show (Null AST.TypeInt) = printf "Null *Type(int)"
    show (Null AST.TypeIntArray) = printf "Null *Type(int[])"
    show (Null (AST.TypeObject name)) = printf "Null *Type(%s)" name
    show (SInt v) = printf "Int *%s" (show v)
    show (SBoolean v) = printf "Boolean *%s" (if v then "true" else "false")
    show (NewObj name) = printf "NewObj *%s" name
    show (NewIntArray s) = printf "NewIntArray *%s" (show s)
    show (Label label) = printf "Label *%s" label
    show (Goto label) = printf "Goto *%s" label
    show (Branch s label) = printf "Branch %s *%s" (show s) label
    show (NBranch s label) = printf "NBranch %s *%s" (show s) label
    show (Call cName s name args) = printf "Call %s *%s(%s)" (show s) name (intercalate ", " $ map show args)
    show (Print s) = printf "Print %s" (show s)
    show (Return s) = printf "Return %s" (show s)
    show (MemberGet cName s name) = printf "Member %s *%s" (show s) name
    show (IndexGet a i) = printf "IndexGet %s %s" (show a) (show i)
    show (Store s i) = printf "Store %s *%s" (show s) (show i)
    show (Load i) = printf "Load *%s" (show i)
    show (VarAssg name s) = printf "VarAssg %s *%s" (show s) name
    show (MemberAssg cName object fName value) = printf "MemberAssg %s %s *%s" (show object) (show value) fName
    show (IndexAssg array value index) = printf "IndexAssg %s %s *%s" (show array) (show value) (show index)
    show (Not s) = printf "Not %s" (show s)
    show (Lt sl sr) = printf "Lt %s %s" (show sl) (show sr)
    show (Le sl sr) = printf "Le %s %s" (show sl) (show sr)
    show (Eq sl sr) = printf "Eq %s %s" (show sl) (show sr)
    show (Ne sl sr) = printf "Ne %s %s" (show sl) (show sr)
    show (Gt sl sr) = printf "Gt %s %s" (show sl) (show sr)
    show (Ge sl sr) = printf "Ge %s %s" (show sl) (show sr)
    show (And sl sr) = printf "And %s %s" (show sl) (show sr)
    show (Or sl sr) = printf "Or %s %s" (show sl) (show sr)
    show (Plus sl sr) = printf "Plus %s %s" (show sl) (show sr)
    show (Minus sl sr) = printf "Minus %s %s" (show sl) (show sr)
    show (Mul sl sr) = printf "Mul %s %s" (show sl) (show sr)
    show (Div sl sr) = printf "Div %s %s" (show sl) (show sr)
    show (Mod sl sr) = printf "Mod %s %s" (show sl) (show sr)
