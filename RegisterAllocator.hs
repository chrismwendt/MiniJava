{-# LANGUAGE TemplateHaskell #-}

module RegisterAllocator where

import qualified SSA as S
import qualified SSARegisters as R
import Data.Functor
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.SetMap as SM
import qualified Data.Set as Set
import Data.Maybe
import Control.Lens
import qualified Data.Graph.Inductive as G
import Data.List
import qualified Data.IntDisjointSet as DJ

type VID = Int

data LiveLabel = LiveLabel
    { _lSt  :: R.Statement
    , _lDef :: Maybe   R.Register
    , _lUse :: Set.Set R.Register
    , _lIn  :: Set.Set R.Register
    , _lOut :: Set.Set R.Register
    }
    deriving (Show, Eq)

makeLenses ''LiveLabel

allocate :: Int -> S.Program -> R.Program
allocate = allocateProgram

allocateProgram :: Int -> S.Program -> R.Program
allocateProgram n p@(S.Program m cs) = R.Program (allocateClass n p m) (map (allocateClass n p) cs)

allocateClass :: Int -> S.Program -> S.Class -> R.Class
allocateClass n program c@(S.Class name fs ms) = R.Class name fs (map (allocateMethod n program c) ms)

allocateMethod :: Int -> S.Program -> S.Class -> S.Method -> R.Method
allocateMethod n program c (S.Method name graph) = squashed
    where
    singles = foldr DJ.insert DJ.empty (G.nodes graph)
    groups = foldr (uncurry DJ.union) singles [(s, o) | (s, S.Unify l r) <- G.labNodes graph, o <- [l, r]]
    translate = fromJust . fst . flip DJ.lookup groups
    unifyRegs (ins, n, s, outs) = case withRegister s of
        Left f ->   (ins, n, f translate (translate n), outs)
        Right s' -> (ins, n, s' translate             , outs)
    graph' = G.gmap unifyRegs (removeUnifies graph)
    squashed = squashRegs n $ allocateMethod' 0 n program c (R.Method name graph')

squashRegs :: Int -> R.Method -> R.Method
squashRegs n (R.Method name g) = R.Method name g'
    where
    lGraph = liveness g
    iGraph = interference lGraph
    regMap = select n iGraph (map snd $ G.labNodes iGraph)
    g' = G.nmap (\s -> R.mapRegs (regMap M.!) s) g

allocateMethod' :: Int -> Int -> S.Program -> S.Class -> R.Method -> R.Method
allocateMethod' spillCount n program c (R.Method name graph) = case spillMaybe of
    Nothing -> R.Method name graph
    Just v -> allocateMethod' (succ spillCount) n program c (R.Method name (strip $ performSpill spillCount v lGraph))
    where
    lGraph = liveness graph
    spillMaybe = find ((> n) . Set.size . _lOut . snd) $ G.labNodes lGraph

strip g = G.nmap _lSt g

maybeToSet = Set.fromList . maybeToList

interference :: G.Gr LiveLabel S.EdgeType -> G.Gr R.Register ()
interference g = live
    where
    concatSet :: Ord a => Set.Set (Set.Set a) -> Set.Set a
    concatSet = Set.foldr Set.union Set.empty
    groups :: Set.Set (Set.Set Int)
    groups = Set.fromList $ map (\(_, label) -> ((maybeToSet $ label ^. lDef) `Set.union` (label ^. lIn)) `Set.union` (label ^. lOut)) $ G.labNodes g
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
    case Set.toList (Set.fromList [0 .. n - 1] `Set.difference` Set.fromList (catMaybes $ map snd $ map (fromJust . G.lab graph) (G.neighbors graph r))) of
        [] -> return Nothing
        (r':_) -> do
            modify $ G.nmap (\(a, o) -> if a == r then (a, Just r') else (a, o))
            return $ Just r'

performSpill :: Int -> (G.Node, LiveLabel) -> G.Gr LiveLabel S.EdgeType -> G.Gr LiveLabel S.EdgeType
performSpill sc (node, label)  g = case Set.toList $ (label ^. lOut) `Set.difference` (maybeToSet (label ^. lDef)) of
    [] -> error "no room"
    (toSpill:_) -> doSpill sc toSpill g

doSpill :: Int -> R.Register -> G.Gr LiveLabel S.EdgeType -> G.Gr LiveLabel S.EdgeType
doSpill sc r g = g''
    where
    g' = execState (mapM (doLoad sc r) $ filter (\n -> r `Set.member` ((fromJust $ G.lab g n) ^. lUse)) (G.nodes g)) g
    g'' = execState (mapM (doStore sc r) $ filter (\n -> r `Set.member` (maybeToSet $ (fromJust $ G.lab g n) ^. lDef)) (G.nodes g)) g'

doLoad :: Int -> R.Register -> G.Node -> State (G.Gr LiveLabel S.EdgeType) ()
doLoad sc r n = do
    g <- get
    case G.match n g of
        (Nothing, _) -> error "match failure"
        (Just (ins, n, LiveLabel stu ds us vIns vOuts, outs), g') -> do
            -- TODO consider using a fresh register since they get squashed anyway
            let avail = case Set.toList $ (Set.fromList [def | (_, LiveLabel { _lDef = Just def }) <- G.labNodes g]) `Set.difference` (Set.delete r vIns) of
                                [] -> error "no available register"
                                (a:_) -> a
            let load = (ins, head (G.newNodes 1 g), LiveLabel (R.Load sc avail) (Just avail) us vIns vOuts, [])
            put (([(S.Step, G.node' load)], n, LiveLabel (R.mapRegs (\x -> if x == r then avail else x) stu) ds us vIns vOuts, outs) G.& (load G.& g'))

doStore :: Int -> R.Register -> G.Node -> State (G.Gr LiveLabel S.EdgeType) ()
doStore sc r n = do
    g <- get
    case G.match n g of
        (Nothing, _) -> error "match failure"
        (Just (ins, n, label@(LiveLabel stu ds us vIns vOuts), outs), g') -> do
            let store = ([(S.Step, n)], head (G.newNodes 1 g), LiveLabel (R.Store r sc) ds us vIns vOuts,  outs)
            put (store G.& ((ins, n, label, []) G.& g'))

liveness :: G.Gr R.Statement S.EdgeType -> G.Gr LiveLabel S.EdgeType
liveness g = graph'
    where
    lGraph = map fst $ linear g
    initialGraph = G.gmap (\(ins, n, s, outs) -> (ins, n, LiveLabel s (R.def s) (R.uses s) Set.empty Set.empty, outs)) g
    graph' = snd $ until (\(old, new) -> old == new) f (f (initialGraph, initialGraph))
    f (prevOld, prevNew) = (prevNew, f' prevNew)
    f' g = foldr f'' g lGraph
    f'' n g = case G.match n g of
        (Nothing, _) -> error "match failure"
        (Just (ins, _, LiveLabel s ds us vIns vOuts, outs), g') ->
            let vIns' = us `Set.union` (vOuts `Set.difference` (maybeToSet ds))
                (_, _, _, fullOuts) = G.context g n
                succVIns = map (_lIn . G.lab' . G.context g) (map snd fullOuts)
                vOuts' = (maybeToSet ds) `Set.union` (foldr Set.union Set.empty succVIns)
                s' = LiveLabel s ds us vIns' vOuts'
                newContext = (ins, n, s', outs)
            in newContext G.& g'

withRegister :: S.Statement -> Either ((S.ID -> R.Register) -> S.ID -> R.Statement) ((S.ID -> R.Register) -> R.Statement)
withRegister (S.Load offset)            = Left  $ \f -> R.Load offset
withRegister (S.Null t)                 = Left  $ \f -> R.Null t
withRegister (S.NewObj s1)              = Left  $ \f -> R.NewObj s1
withRegister (S.NewIntArray i1)         = Left  $ \f -> R.NewIntArray (f i1)
withRegister (S.This)                   = Left  $ \f -> R.This
withRegister (S.SInt v)                 = Left  $ \f -> R.SInt v
withRegister (S.SBoolean v)             = Left  $ \f -> R.SBoolean v
withRegister (S.Parameter position)     = Left  $ \f -> R.Parameter position
withRegister (S.Call s1 i1 s2 is)       = Left  $ \f -> R.Call s1 (f i1) s2 (map f is)
withRegister (S.MemberGet s1 i1 s2)     = Left  $ \f -> R.MemberGet s1 (f i1) s2
withRegister (S.MemberAssg s1 i1 s2 i2) = Left  $ \f -> R.MemberAssg s1 (f i1) s2 (f i2)
withRegister (S.VarAssg i1)             = Left  $ \f -> R.VarAssg (f i1)
withRegister (S.IndexGet i1 i2)         = Left  $ \f -> R.IndexGet (f i1) (f i2)
withRegister (S.IndexAssg i1 i2 i3)     = Left  $ \f -> R.IndexAssg (f i1) (f i2) (f i3)
withRegister (S.ArrayLength i1)         = Left  $ \f -> R.ArrayLength (f i1)
withRegister (S.Not i1)                 = Left  $ \f -> R.Not (f i1)
withRegister (S.Lt i1 i2)               = Left  $ \f -> R.Lt (f i1) (f i2)
withRegister (S.Le i1 i2)               = Left  $ \f -> R.Le (f i1) (f i2)
withRegister (S.Eq i1 i2)               = Left  $ \f -> R.Eq (f i1) (f i2)
withRegister (S.Ne i1 i2)               = Left  $ \f -> R.Ne (f i1) (f i2)
withRegister (S.Gt i1 i2)               = Left  $ \f -> R.Gt (f i1) (f i2)
withRegister (S.Ge i1 i2)               = Left  $ \f -> R.Ge (f i1) (f i2)
withRegister (S.And i1 i2)              = Left  $ \f -> R.And (f i1) (f i2)
withRegister (S.Or i1 i2)               = Left  $ \f -> R.Or (f i1) (f i2)
withRegister (S.Plus i1 i2)             = Left  $ \f -> R.Plus (f i1) (f i2)
withRegister (S.Minus i1 i2)            = Left  $ \f -> R.Minus (f i1) (f i2)
withRegister (S.Mul i1 i2)              = Left  $ \f -> R.Mul (f i1) (f i2)
withRegister (S.Div i1 i2)              = Left  $ \f -> R.Div (f i1) (f i2)
withRegister (S.Mod i1 i2)              = Left  $ \f -> R.Mod (f i1) (f i2)
withRegister (S.Store i1 offset)        = Right $ \f -> R.Store (f i1) offset
withRegister (S.Branch i1)              = Right $ \f -> R.Branch (f i1)
withRegister (S.NBranch i1)             = Right $ \f -> R.NBranch (f i1)
withRegister (S.Arg i1 p)               = Right $ \f -> R.Arg (f i1) p
withRegister (S.Return i1)              = Right $ \f -> R.Return (f i1)
withRegister (S.Print i1)               = Right $ \f -> R.Print (f i1)
withRegister (S.BeginMethod)            = Right $ \f -> R.BeginMethod
withRegister (S.Label)                  = Right $ \f -> R.Label
withRegister (S.Goto)                   = Right $ \f -> R.Goto
withRegister (S.Unify _ _)              = error "withRegister of Unify"

linear :: G.Gr R.Statement S.EdgeType -> [(G.Node, R.Statement)]
linear g = snd $ evalState (runWriterT (doLinear begin)) Set.empty
    where
    begin = head [n | (n, R.BeginMethod) <- G.labNodes g]
    doLinear node = do
        seen <- get
        when (node `Set.notMember` seen) $ do
            let (_, _, statement, outs) = G.context g node
                filterEdge edge = map snd . filter ((== edge) . fst)
            tell [(node, statement)]
            modify $ Set.insert node
            mapM_ doLinear (filterEdge S.Step outs)
            mapM_ doLinear (filterEdge S.Jump outs)

removeUnifies :: G.Gr S.Statement S.EdgeType -> G.Gr S.Statement S.EdgeType
removeUnifies g = case [n | (n, S.Unify _ _) <- G.labNodes g] of
    [] -> g
    (n : _) -> let (ins, _, _, outs) = G.context g n
                   some edge = snd . fromJust . find ((== edge) . fst)
               in removeUnifies $ G.insEdge (some S.Step ins, some S.Step outs, S.Step) (G.delNode n g)
