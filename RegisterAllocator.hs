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
import Data.Tuple

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
-- aProgram n p@(S.Program m cs) = R.Program (aClass n p m) (map (aClass n p) cs)
aProgram n p@(S.Program m cs) = R.Program (aClass n p (head cs)) (map (aClass n p) [])

aClass :: Int -> S.Program -> S.Class -> R.Class
aClass n program c@(S.Class name fs ms) = R.Class name fs (map (aMethod n program c) ms)

aMethod :: Int -> S.Program -> S.Class -> S.Method -> R.Method
aMethod n program c (S.Method name graph) = squashRegs n $ aMethod' 0 n program c (R.Method name graph')
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
    graph' = G.gmap conversion (ununify graph)

squashRegs :: Int -> R.Method -> R.Method
squashRegs n (R.Method name g) = R.Method name g'
    where
    lGraph = liveness g
    iGraph = interference lGraph
    regMap = select n iGraph (map snd $ G.labNodes iGraph)
    g' = G.nmap (\s -> mapRegs (regMap M.!) s) g

aMethod' :: Int -> Int -> S.Program -> S.Class -> R.Method -> R.Method
aMethod' spillCount n program c (R.Method name graph) = case spillMaybe of
    Nothing -> R.Method name graph
    Just v -> aMethod' (succ spillCount) n program c (R.Method name (strip $ performSpill spillCount v lGraph))
    where
    lGraph = liveness graph
    spillMaybe = find (\(_, (_, _, _, _, vOuts)) -> Set.size vOuts > n) $ G.labNodes lGraph

strip g = G.nmap (\(st, ds, us, vIns, vOuts) -> st) g

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

select :: Int -> G.Gr R.Register () -> [R.Register] -> M.Map R.Register R.Register
select n graph regs = M.fromList $ mapMaybe f $ zip regs $ evalState (mapM (select' n) regs) (G.nmap (\a -> (a, Nothing)) graph)
    where
    f (r, Just r') = Just (r, r')
    f (_, Nothing) = Nothing

select' :: Int -> R.Register -> State (G.Gr (R.Register, Maybe R.Register) ()) (Maybe R.Register)
select' n r = do
    graph <- get
    case Set.toList (Set.fromList [1 .. n] `Set.difference` Set.fromList (catMaybes $ map snd $ map (fromJust . G.lab graph) (G.neighbors graph r))) of
        [] -> return Nothing
        (r':_) -> do
            modify $ G.nmap (\(a, o) -> if a == r then (a, Just r') else (a, o))
            return $ Just r'

performSpill :: Int -> (G.Node, (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register)) -> G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType -> G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType
performSpill sc (node, (st, ds, us, vIns, vOuts))  g = case Set.toList $ vOuts `Set.difference` ds of
    [] -> error "no room"
    (toSpill:_) -> doSpill sc toSpill g

doSpill :: Int -> R.Register -> G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType -> G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType
doSpill sc r g = g''
    where
    g' = execState (mapM (doLoad sc r) $ filter (\n -> r `Set.member` (let (_, _, vUses, _, _) = fromJust $ G.lab g n in vUses)) (G.nodes g)) g
    g'' = execState (mapM (doStore sc r) $ filter (\n -> r `Set.member` (let (_, vDefs, _, _, _) = fromJust $ G.lab g n in vDefs)) (G.nodes g)) g'

doLoad :: Int -> R.Register -> G.Node -> State (G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType) ()
doLoad sc r n = do
    g <- get
    case G.match n g of
        (Nothing, _) -> error "match failure"
        (Just (ins, n, st@(stu, ds, us, vIns, vOuts), outs), g') -> do
            let avail = case Set.toList $ (foldr Set.union Set.empty $ map (\(_, (s, _, _, _, _)) -> def s) (G.labNodes g)) `Set.difference` vIns of
                                [] -> error "no available register"
                                (a:_) -> a
            let load = (ins, head (G.newNodes 1 g), (R.Load sc avail, ds, us, vIns, vOuts), [])
            put (([(S.Step, G.node' load)], n, (mapRegs (\x -> if x == r then avail else x) stu, ds, us, vIns, vOuts), outs) G.& (load G.& g'))

doStore :: Int -> R.Register -> G.Node -> State (G.Gr (R.Statement, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register, Set.Set R.Register) S.EdgeType) ()
doStore sc r n = do
    g <- get
    case G.match n g of
        (Nothing, _) -> error "match failure"
        (Just (ins, n, st@(stu, ds, us, vIns, vOuts), outs), g') -> do
            let store = ([(S.Step, n)], head (G.newNodes 1 g), (R.Store r sc, ds, us, vIns, vOuts),  outs)
            put (store G.& ((ins, n, st, []) G.& g'))

        -- let doStore node = do
        --     (g, sc) <- get
        --     case G.match node g of
        --         (Nothing, _) -> error "match failure"
        --         (Just (ins, n, st, outs), g') -> do
        --             let store = ([(S.Step, n)], head (G.newNodes 1 g), R.Store reg sc, outs)
        --             put (store G.& ((ins, n, st, []) G.& g'), sc)
        -- let doLoad node = do
        --     (g, sc) <- get
        --     case G.match node g of
        --         (Nothing, _) -> error "match failure"
        --         (Just (ins, n, st, outs), g') -> do
        --             let load = (ins, head (G.newNodes 1 g), R.Load sc reg, [])
        --             put (([(S.Step, G.node' load)], n, st, outs) G.& (load G.& g'), sc)
        -- mapM doStore (Set.toList $ SM.lookup reg regToNodes)
        --         mapM doLoad (map fst $ filter (\(n, st) -> reg `Set.member` vUses st) (G.labNodes g))

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

mapRegs f (R.Load offset r)            = R.Load offset (f r)
mapRegs f (R.Null t r)                 = R.Null t (f r)
mapRegs f (R.NewObj s1 r)              = R.NewObj s1 (f r)
mapRegs f (R.NewIntArray r1 r)         = R.NewIntArray (f r1) (f r)
mapRegs f (R.This r)                   = R.This (f r)
mapRegs f (R.SInt v r)                 = R.SInt v (f r)
mapRegs f (R.SBoolean v r)             = R.SBoolean v (f r)
mapRegs f (R.Parameter position r)     = R.Parameter position (f r)
mapRegs f (R.Call s1 r1 s2 is r)       = R.Call s1 (f r1) s2 is (f r)
mapRegs f (R.MemberGet s1 r1 s2 r)     = R.MemberGet s1 (f r1) s2 (f r)
mapRegs f (R.MemberAssg s1 r1 s2 r2 r) = R.MemberAssg s1 (f r1) s2 (f r2) (f r)
mapRegs f (R.VarAssg r1 r)             = R.VarAssg (f r1) (f r)
mapRegs f (R.IndexGet r1 r2 r)         = R.IndexGet (f r1) (f r2) (f r)
mapRegs f (R.IndexAssg r1 r2 r3 r)     = R.IndexAssg (f r1) (f r2) (f r3) (f r)
mapRegs f (R.Not r1 r)                 = R.Not (f r1) (f r)
mapRegs f (R.Lt r1 r2 r)               = R.Lt (f r1) (f r2) (f r)
mapRegs f (R.Le r1 r2 r)               = R.Le (f r1) (f r2) (f r)
mapRegs f (R.Eq r1 r2 r)               = R.Eq (f r1) (f r2) (f r)
mapRegs f (R.Ne r1 r2 r)               = R.Ne (f r1) (f r2) (f r)
mapRegs f (R.Gt r1 r2 r)               = R.Gt (f r1) (f r2) (f r)
mapRegs f (R.Ge r1 r2 r)               = R.Ge (f r1) (f r2) (f r)
mapRegs f (R.And r1 r2 r)              = R.And (f r1) (f r2) (f r)
mapRegs f (R.Or r1 r2 r)               = R.Or (f r1) (f r2) (f r)
mapRegs f (R.Plus r1 r2 r)             = R.Plus (f r1) (f r2) (f r)
mapRegs f (R.Minus r1 r2 r)            = R.Minus (f r1) (f r2) (f r)
mapRegs f (R.Mul r1 r2 r)              = R.Mul (f r1) (f r2) (f r)
mapRegs f (R.Div r1 r2 r)              = R.Div (f r1) (f r2) (f r)
mapRegs f (R.Mod r1 r2 r)              = R.Mod (f r1) (f r2) (f r)
mapRegs f (R.Store r1 offset)          = R.Store (f r1) offset
mapRegs f (R.Branch r1)                = R.Branch (f r1)
mapRegs f (R.NBranch r1)               = R.NBranch (f r1)
mapRegs f (R.Arg r1 p)                 = R.Arg (f r1) p
mapRegs f (R.Return r1)                = R.Return (f r1)
mapRegs f (R.Print r1)                 = R.Print (f r1)
mapRegs f (R.BeginMethod)              = R.BeginMethod
mapRegs f (R.Label)                    = R.Label
mapRegs f (R.Goto)                     = R.Goto

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
