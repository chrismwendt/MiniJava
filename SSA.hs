{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module SSA where

import qualified AST as AST
import qualified ASTTyped as T
import Data.Functor
import Data.Bifunctor
import qualified Data.Map as M
import Control.Lens
import Text.Printf
import Data.List

type ID = Int

data Program info ref = Program
    { _pProgram :: T.Program
    , _pMain :: [ref]
    , _pClasses :: [Class info ref]
    }

data Class info ref = Class
    { _cClass :: T.Class
    , _cFields :: [Field info]
    , _cMethod :: [Method info ref]
    }

data Field info = Field
    { _fVariable :: AST.Variable
    , _fPosition :: Int
    , _fInfo :: info
    }
    deriving (Functor)

data Method info ref = Method
    { _mMethod :: T.Method
    , _mParameters :: [ref]
    , _mStatements :: [ref]
    , _mReturn :: ref
    }

data Statement info ref = Statement
    { _sOp :: Op ref
    , _sInfo :: info
    }
    deriving (Eq)

data Op ref =
      Unify ref ref
    | Alias ref
    | This
    | Variable String Int
    | Arg ref Int
    | Null AST.Type
    | SInt Int
    | SBoolean Bool
    | NewObj String
    | NewIntArray ref
    | Label String
    | Goto String
    | Branch ref String
    | NBranch ref String
    | Call String ref String [ref]
    | Print ref
    | Return ref
    | MemberGet String ref String
    | IndexGet ref ref
    | Store ref Int
    | Load Int
    | VarAssg String ref
    | MemberAssg String ref String ref
    | IndexAssg ref ref ref
    | Not ref
    | Lt ref ref
    | Le ref ref
    | Eq ref ref
    | Ne ref ref
    | Gt ref ref
    | Ge ref ref
    | And ref ref
    | Or ref ref
    | Plus ref ref
    | Minus ref ref
    | Mul ref ref
    | Div ref ref
    | Mod ref ref
    deriving (Eq, Functor)

makeLenses ''Program
makeLenses ''Class
makeLenses ''Field
makeLenses ''Method
makeLenses ''Statement

instance (Show ref, Show info) => Show (Program info ref) where
    show (Program _ ss cs) = printf "program:\n  main:\n    method main:\n%s%s" (concatMap show ss) (concatMap show cs)

instance (Show ref, Show info) => Show (Method info ref) where
    show (Method (T.Method _ name _ _ _ _) ps ss _) = printf "    method %s:\n%s" name (concatMap show ss)

instance (Show ref, Show info) => Show (Class info ref) where
    show (Class (T.Class name _ _ _) _ ms) = printf "  class %s:\n%s" name (concatMap show ms)

instance Show info => Show (Field info) where
    show (Field (AST.Variable _ name) _ _) = name

instance (Show ref, Show info) => Show (Statement info ref) where
    show (Statement op info) = printf "      ?: %s :%s\n" (show op) (show info)

instance Show ref => Show (Op ref) where
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

instance Bifunctor Program where
    bimap f g (Program ast rs cs) = Program ast (map g rs) (map (bimap f g) cs)

instance Bifunctor Class where
    bimap f g (Class cd fs ms) = Class cd (map (fmap f) fs) (map (bimap f g) ms)

instance Bifunctor Method where
    bimap f g (Method md ps ss r) = Method md (map g ps) (map g ss) (g r)

instance Bifunctor Statement where
    bimap f g (Statement op info) = Statement (fmap g op) (f info)


