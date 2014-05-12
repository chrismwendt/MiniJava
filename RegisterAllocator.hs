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
import qualified Data.Set as Set
import Data.Maybe
import Control.Lens
import qualified Data.Graph.Inductive as G
import Data.List
import Data.Ord
import Safe
import Debug.Trace
import Text.Show.Pretty

bug x = trace (ppShow x) x
bug' x y = trace (ppShow x) y

type VID = Int

data RState = RState
    { _stSIDToVID :: M.Map S.ID VID
    , _stVIDToVar :: SM.SetMap VID S.ID
    }

data CFLabel = CFLabel
    { _cfDef :: VID
    , _cfUse :: Set.Set VID
    , _cfIn :: Set.Set VID
    , _cfOut :: Set.Set VID
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
-- aMethod' n program c (R.Method name graph) = if null spills
--     then R.Method name graph
--     else aMethod' n program c (R.Method name (performSpills spills graph))
--     where
--     spills = select n $ simplify n $ interference $ liveness graph
aMethod' n program c (R.Method name graph) = R.Method name (bug' (graph, liveness graph, interference $ liveness graph, simplify n $ interference $ liveness graph) graph)

liveness :: G.Gr R.Statement S.EdgeType -> G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType
liveness g = graph'
    where
    lGraph = map fst $ linear g
    initialGraph = G.gmap (\(ins, n, s, outs) -> (ins, n, (s, def s, vUses s, Set.empty, Set.empty), outs)) g
    graph' = snd $ until (\(old, new) -> old == new) f (f (initialGraph, initialGraph))
    f (prevOld, prevNew) = (prevNew, f' prevNew)
    f' g = foldr f'' g lGraph
    f'' n g = case G.match n g of
        (Nothing, _) -> error "match failure"
        (Just (ins, _, (s, ds, us, vIns, vOuts), outs), g') ->
            let vIns' = us `Set.union` (vOuts `Set.difference` ds)
                (_, _, _, fullOuts) = G.context g n
                succVIns = map ((\(_, _, (_, _, _, sVIns, _), _) -> sVIns) . G.context g) (map snd fullOuts)
                vOuts' = ds `Set.union` (foldr Set.union Set.empty succVIns)
                s' = (s, ds, us, vIns', vOuts')
                newContext = (ins, n, s', outs)
            in newContext G.& g'

interference :: G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType -> G.Gr R.Register ()
interference g = live
    where
    concatSet :: Ord a => Set.Set (Set.Set a) -> Set.Set a
    concatSet = Set.foldr Set.union Set.empty
    groups :: Set.Set (Set.Set Int)
    groups = Set.fromList $ map (vInsOf . snd) $ G.labNodes g
    allVars = concatSet groups
    h :: Int -> Set.Set Int -> Set.Set (Int, Int)
    h var gr = if Set.member var gr
        then Set.map (\x -> (var, x)) (Set.delete var gr)
        else Set.empty
    f :: Int -> Set.Set (Int, Int)
    f var = concatSet $ Set.map (h var) groups
    edgeSet = concatSet $ Set.map f allVars
    vInsOf (_, _, _, vIns, _) = vIns
    live = G.mkGraph (map (\v -> (v, v)) $ Set.toList allVars) (map (\(from, to) -> (from, to, ())) $ Set.toList edgeSet)

simplify :: Int -> G.Gr R.Register () -> [R.Register]
simplify n graph = regs graph
    where
    regs g
        | G.isEmpty g = []
        | otherwise = case find ((< n) . G.indeg g . fst) (G.labNodes g) of
            Just (node, reg) -> reg : regs (G.delNode node g)
            Nothing -> let ((_, _, reg, _), g') = G.matchAny g in reg : regs g'

select = undefined

performSpills = undefined

withRegister :: S.Statement -> Maybe (Either ((S.ID -> R.Register) -> S.ID -> R.Statement) ((S.ID -> R.Register) -> R.Statement))
withRegister (S.Load offset)            = Just $ Left  $ \f -> R.Load offset
withRegister (S.Null t)                 = Just $ Left  $ \f -> R.Null t
withRegister (S.NewObj s1)              = Just $ Left  $ \f -> R.NewObj s1
withRegister (S.NewIntArray i1)         = Just $ Left  $ \f -> R.NewIntArray (f i1)
withRegister (S.This)                   = Just $ Left  $ \f -> R.This
withRegister (S.SInt v)                 = Just $ Left  $ \f -> R.SInt v
withRegister (S.SBoolean v)             = Just $ Left  $ \f -> R.SBoolean v
withRegister (S.Parameter position)     = Just $ Left  $ \f -> R.Parameter position
withRegister (S.Call s1 i1 s2 is)       = Just $ Left  $ \f -> R.Call s1 (f i1) s2 (map f is)
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

vUses :: R.Statement -> Set.Set R.Register
vUses (R.Load offset r)            = Set.fromList []
vUses (R.Null t r)                 = Set.fromList []
vUses (R.NewObj s1 r)              = Set.fromList []
vUses (R.NewIntArray r1 r)         = Set.fromList [r1]
vUses (R.This r)                   = Set.fromList []
vUses (R.SInt v r)                 = Set.fromList []
vUses (R.SBoolean v r)             = Set.fromList []
vUses (R.Parameter position r)     = Set.fromList []
vUses (R.Call s1 r1 s2 is r)       = Set.fromList (r1 : is)
vUses (R.MemberGet s1 r1 s2 r)     = Set.fromList [r1]
vUses (R.MemberAssg s1 r1 s2 r2 r) = Set.fromList [r1, r2]
vUses (R.VarAssg r1 r)             = Set.fromList [r1]
vUses (R.IndexGet r1 r2 r)         = Set.fromList [r1, r2]
vUses (R.IndexAssg r1 r2 r3 r)     = Set.fromList [r1, r2, r3]
vUses (R.Not r1 r)                 = Set.fromList [r1]
vUses (R.Lt r1 r2 r)               = Set.fromList [r1, r2]
vUses (R.Le r1 r2 r)               = Set.fromList [r1, r2]
vUses (R.Eq r1 r2 r)               = Set.fromList [r1, r2]
vUses (R.Ne r1 r2 r)               = Set.fromList [r1, r2]
vUses (R.Gt r1 r2 r)               = Set.fromList [r1, r2]
vUses (R.Ge r1 r2 r)               = Set.fromList [r1, r2]
vUses (R.And r1 r2 r)              = Set.fromList [r1, r2]
vUses (R.Or r1 r2 r)               = Set.fromList [r1, r2]
vUses (R.Plus r1 r2 r)             = Set.fromList [r1, r2]
vUses (R.Minus r1 r2 r)            = Set.fromList [r1, r2]
vUses (R.Mul r1 r2 r)              = Set.fromList [r1, r2]
vUses (R.Div r1 r2 r)              = Set.fromList [r1, r2]
vUses (R.Mod r1 r2 r)              = Set.fromList [r1, r2]
vUses (R.Store r1 offset)          = Set.fromList [r1]
vUses (R.Branch r1)                = Set.fromList [r1]
vUses (R.NBranch r1)               = Set.fromList [r1]
vUses (R.Arg r1 p)                 = Set.fromList [r1]
vUses (R.Return r1)                = Set.fromList [r1]
vUses (R.Print r1)                 = Set.fromList [r1]
vUses (R.BeginMethod)              = Set.fromList []
vUses (R.Label)                    = Set.fromList []
vUses (R.Goto)                     = Set.fromList []

def :: R.Statement -> Set.Set R.Register
def (R.Load offset r)            = Set.fromList [r]
def (R.Null t r)                 = Set.fromList [r]
def (R.NewObj s1 r)              = Set.fromList [r]
def (R.NewIntArray r1 r)         = Set.fromList [r]
def (R.This r)                   = Set.fromList [r]
def (R.SInt v r)                 = Set.fromList [r]
def (R.SBoolean v r)             = Set.fromList [r]
def (R.Parameter position r)     = Set.fromList [r]
def (R.Call s1 r1 s2 is r)       = Set.fromList [r]
def (R.MemberGet s1 r1 s2 r)     = Set.fromList [r]
def (R.MemberAssg s1 r1 s2 r2 r) = Set.fromList [r]
def (R.VarAssg r1 r)             = Set.fromList [r]
def (R.IndexGet r1 r2 r)         = Set.fromList [r]
def (R.IndexAssg r1 r2 r3 r)     = Set.fromList [r]
def (R.Not r1 r)                 = Set.fromList [r]
def (R.Lt r1 r2 r)               = Set.fromList [r]
def (R.Le r1 r2 r)               = Set.fromList [r]
def (R.Eq r1 r2 r)               = Set.fromList [r]
def (R.Ne r1 r2 r)               = Set.fromList [r]
def (R.Gt r1 r2 r)               = Set.fromList [r]
def (R.Ge r1 r2 r)               = Set.fromList [r]
def (R.And r1 r2 r)              = Set.fromList [r]
def (R.Or r1 r2 r)               = Set.fromList [r]
def (R.Plus r1 r2 r)             = Set.fromList [r]
def (R.Minus r1 r2 r)            = Set.fromList [r]
def (R.Mul r1 r2 r)              = Set.fromList [r]
def (R.Div r1 r2 r)              = Set.fromList [r]
def (R.Mod r1 r2 r)              = Set.fromList [r]
def (R.Store r1 offset)          = Set.fromList []
def (R.Branch r1)                = Set.fromList []
def (R.NBranch r1)               = Set.fromList []
def (R.Arg r1 p)                 = Set.fromList []
def (R.Return r1)                = Set.fromList []
def (R.Print r1)                 = Set.fromList []
def (R.BeginMethod)              = Set.fromList []
def (R.Label)                    = Set.fromList []
def (R.Goto)                     = Set.fromList []

linear :: G.Gr R.Statement S.EdgeType -> [(G.Node, R.Statement)]
linear g = linear' g start Nothing
    where
    startMaybe = G.ufold (\(_, n, l, _) acc -> case l of { R.BeginMethod -> Just n; _ -> acc }) Nothing g
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
