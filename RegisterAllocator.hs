{-# LANGUAGE TemplateHaskell #-}

module RegisterAllocator where

import qualified ASTTyped as T
import qualified AST
import qualified SSA as S
import qualified SSARegisters as R
import Data.Functor
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.SetMap as SM
import qualified Data.Set as S
import Data.Maybe
import Control.Lens
import qualified Data.Graph.Inductive as G
import Data.List
import Data.Ord
import Safe

type VID = Int

data RState = RState
    { _stSIDToVID :: M.Map S.ID VID
    , _stVIDToVar :: SM.SetMap VID S.ID
    }

data CFLabel = CFLabel
    { _cfDef :: VID
    , _cfUse :: S.Set VID
    , _cfIn :: S.Set VID
    , _cfOut :: S.Set VID
    }

makeLenses ''RState

allocate :: Int -> S.Program -> R.Program
allocate n = aProgram n

aProgram :: Int -> S.Program -> R.Program
aProgram n p@(S.Program m cs) = R.Program (aClass n p m) (map (aClass n p) cs)

aClass :: Int -> S.Program -> S.Class -> R.Class
aClass n program c@(S.Class name fs ms) = R.Class name fs (map (aMethod n program c) ms)

aMethod :: Int -> S.Program -> S.Class -> S.Method -> R.Method
aMethod n program c (S.Method name graph) = aMethod' n program c (R.Method name graph')
    where
    varGraph :: G.Gr S.Statement ()
    varGraph = G.mkGraph
        (G.labNodes graph)
        [(s, o, ()) | (s, S.Unify l r) <- G.labNodes graph, o <- [l, r]]
    varGroups = M.fromList
        $ concatMap (\(ns, v) -> zip ns (repeat v))
        $ zip (G.components varGraph) [0 .. ]
    conversion (ins, n, s, outs) = case withRegister s of
        Nothing -> error "withRegister failed"
        Just (Left f) -> (ins, n, f (varGroups M.!) (varGroups M.! n), outs)
        Just (Right s') -> (ins, n, s' (varGroups M.!), outs)
    graph' = (G.gmap conversion (ununify graph))

aMethod' :: Int -> S.Program -> S.Class -> R.Method -> R.Method
aMethod' n program c (R.Method name graph) = R.Method name graph

withRegister :: S.Statement -> Maybe (Either ((S.ID -> R.Register) -> S.ID -> R.Statement) ((S.ID -> R.Register) -> R.Statement))
withRegister (S.Load offset)            = Just $ Left  $ \f -> R.Load offset
withRegister (S.Null t)                 = Just $ Left  $ \f -> R.Null t
withRegister (S.NewObj s1)              = Just $ Left  $ \f -> R.NewObj s1
withRegister (S.NewIntArray i1)         = Just $ Left  $ \f -> R.NewIntArray (f i1)
withRegister (S.This)                   = Just $ Left  $ \f -> R.This
withRegister (S.SInt v)                 = Just $ Left  $ \f -> R.SInt v
withRegister (S.SBoolean v)             = Just $ Left  $ \f -> R.SBoolean v
withRegister (S.Parameter position)     = Just $ Left  $ \f -> R.Parameter position
withRegister (S.Call s1 i1 s2 is)       = Just $ Left  $ \f -> R.Call s1 (f i1) s2 is
withRegister (S.MemberGet s1 i1 s2)     = Just $ Left  $ \f -> R.MemberGet s1 (f i1) s2
withRegister (S.MemberAssg s1 i1 s2 i2) = Just $ Left  $ \f -> R.MemberAssg s1 (f i1) s2 (f i2)
withRegister (S.VarAssg i1)             = Just $ Left  $ \f -> R.VarAssg (f i1)
withRegister (S.IndexGet i1 i2)         = Just $ Left  $ \f -> R.IndexGet (f i1) (f i2)
withRegister (S.IndexAssg i1 i2 i3)     = Just $ Left  $ \f -> R.IndexAssg (f i1) (f i2) (f i3)
withRegister (S.Not i1)                 = Just $ Left  $ \f -> R.Not (f i1)
withRegister (S.Lt i1 i2)               = Just $ Left  $ \f -> R.Lt (f i1) (f i2)
withRegister (S.Le i1 i2)               = Just $ Left  $ \f -> R.Le (f i1) (f i2)
withRegister (S.Eq i1 i2)               = Just $ Left  $ \f -> R.Eq (f i1) (f i2)
withRegister (S.Ne i1 i2)               = Just $ Left  $ \f -> R.Ne (f i1) (f i2)
withRegister (S.Gt i1 i2)               = Just $ Left  $ \f -> R.Gt (f i1) (f i2)
withRegister (S.Ge i1 i2)               = Just $ Left  $ \f -> R.Ge (f i1) (f i2)
withRegister (S.And i1 i2)              = Just $ Left  $ \f -> R.And (f i1) (f i2)
withRegister (S.Or i1 i2)               = Just $ Left  $ \f -> R.Or (f i1) (f i2)
withRegister (S.Plus i1 i2)             = Just $ Left  $ \f -> R.Plus (f i1) (f i2)
withRegister (S.Minus i1 i2)            = Just $ Left  $ \f -> R.Minus (f i1) (f i2)
withRegister (S.Mul i1 i2)              = Just $ Left  $ \f -> R.Mul (f i1) (f i2)
withRegister (S.Div i1 i2)              = Just $ Left  $ \f -> R.Div (f i1) (f i2)
withRegister (S.Mod i1 i2)              = Just $ Left  $ \f -> R.Mod (f i1) (f i2)
withRegister (S.Store i1 offset)        = Just $ Right $ \f -> R.Store (f i1) offset
withRegister (S.Branch i1)              = Just $ Right $ \f -> R.Branch (f i1)
withRegister (S.NBranch i1)             = Just $ Right $ \f -> R.NBranch (f i1)
withRegister (S.Arg i1 p)               = Just $ Right $ \f -> R.Arg (f i1) p
withRegister (S.Return i1)              = Just $ Right $ \f -> R.Return (f i1)
withRegister (S.Print i1)               = Just $ Right $ \f -> R.Print (f i1)
withRegister (S.BeginMethod)            = Just $ Right $ \f -> R.BeginMethod
withRegister (S.Label)                  = Just $ Right $ \f -> R.Label
withRegister (S.Goto)                   = Just $ Right $ \f -> R.Goto
withRegister (S.Unify _ _)              = Nothing

uses :: R.Statement -> S.Set R.Register
uses (R.Load offset r)            = S.fromList []
uses (R.Null t r)                 = S.fromList []
uses (R.NewObj s1 r)              = S.fromList []
uses (R.NewIntArray r1 r)         = S.fromList [r1]
uses (R.This r)                   = S.fromList []
uses (R.SInt v r)                 = S.fromList []
uses (R.SBoolean v r)             = S.fromList []
uses (R.Parameter position r)     = S.fromList []
uses (R.Call s1 r1 s2 is r)       = S.fromList [r1]
uses (R.MemberGet s1 r1 s2 r)     = S.fromList [r1]
uses (R.MemberAssg s1 r1 s2 r2 r) = S.fromList [r1, r2]
uses (R.VarAssg r1 r)             = S.fromList [r1]
uses (R.IndexGet r1 r2 r)         = S.fromList [r1, r2]
uses (R.IndexAssg r1 r2 r3 r)     = S.fromList [r1, r2, r3]
uses (R.Not r1 r)                 = S.fromList [r1]
uses (R.Lt r1 r2 r)               = S.fromList [r1, r2]
uses (R.Le r1 r2 r)               = S.fromList [r1, r2]
uses (R.Eq r1 r2 r)               = S.fromList [r1, r2]
uses (R.Ne r1 r2 r)               = S.fromList [r1, r2]
uses (R.Gt r1 r2 r)               = S.fromList [r1, r2]
uses (R.Ge r1 r2 r)               = S.fromList [r1, r2]
uses (R.And r1 r2 r)              = S.fromList [r1, r2]
uses (R.Or r1 r2 r)               = S.fromList [r1, r2]
uses (R.Plus r1 r2 r)             = S.fromList [r1, r2]
uses (R.Minus r1 r2 r)            = S.fromList [r1, r2]
uses (R.Mul r1 r2 r)              = S.fromList [r1, r2]
uses (R.Div r1 r2 r)              = S.fromList [r1, r2]
uses (R.Mod r1 r2 r)              = S.fromList [r1, r2]
uses (R.Store r1 offset)          = S.fromList [r1]
uses (R.Branch r1)                = S.fromList [r1]
uses (R.NBranch r1)               = S.fromList [r1]
uses (R.Arg r1 p)                 = S.fromList [r1]
uses (R.Return r1)                = S.fromList [r1]
uses (R.Print r1)                 = S.fromList [r1]
uses (R.BeginMethod)              = S.fromList []
uses (R.Label)                    = S.fromList []
uses (R.Goto)                     = S.fromList []

linear :: G.Gr S.Statement S.EdgeType -> [(G.Node, S.Statement)]
linear g = linear' g start Nothing
    where
    startMaybe = G.ufold (\(_, n, l, _) acc -> case l of { S.BeginMethod -> Just n; _ -> acc }) Nothing g
    start = case startMaybe of
        Just s -> s
        Nothing -> error "applied linearize to a graph without BeginMethod"
    linear' g n next = case G.context g n of
        (_, _, v, []) -> (n, v) : (case next of
            Nothing -> []
            Just blub -> linear' g blub Nothing)
        (_, _, v, outs) -> (n, v) : (case (find ((== S.Step) . fst) outs, find ((== S.Jump) . fst) outs) of
            (Nothing, Nothing) -> (case next of
                Nothing -> []
                Just blub -> linear' g blub Nothing)
            (Nothing, Just j) -> linear' g (fromJustDef (snd j) next) Nothing
            (Just s, Nothing) -> linear' g (snd s) next
            (Just s, Just j) -> linear' g (snd s) (Just $ fromJustDef (snd j) next))

ununify :: G.Gr S.Statement S.EdgeType -> G.Gr S.Statement S.EdgeType
ununify g = case [n | (n, S.Unify _ _) <- G.labNodes g] of
    [] -> g
    (n:ns) -> let (ins, _, _, outs) = G.context g n
                  some edge = snd . fromJust . find ((== edge) . fst)
              in ununify $ G.insEdge (some S.Step ins, some S.Step outs, S.Step) $ G.delNode n g
