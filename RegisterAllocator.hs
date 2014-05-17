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
import Sandbox
import Control.Monad.Loops

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
allocateProgram nRegs (S.Program m cs) = R.Program (allocateClass nRegs m) (map (allocateClass nRegs) cs)

allocateClass :: Int -> S.Class -> R.Class
allocateClass nRegs (S.Class name fs ms) = R.Class name fs (map (allocateMethod nRegs) ms)

allocateMethod :: Int -> S.Method -> R.Method
allocateMethod nRegs (S.Method name graph) = R.Method name graph'
    where
    graph' = squashRegs nRegs . limitInterference nRegs . assignRegisters $ graph

assignRegisters :: G.Gr S.Statement S.EdgeType -> G.Gr R.Statement S.EdgeType
assignRegisters graph = G.gmap unifyRegs (removeUnifies graph)
    where
    singles = foldr DJ.insert DJ.empty (G.nodes graph)
    variables = foldr (uncurry DJ.union) singles [(s, o) | (s, S.Unify l r) <- G.labNodes graph, o <- [l, r]]
    translate = fromJust . fst . flip DJ.lookup variables
    unifyRegs (ins, n, s, outs) = case R.withRegister s of
        Left s' ->  (ins, n, R.mapRegs translate (s' n), outs)
        Right s' -> (ins, n, R.mapRegs translate s'    , outs)

limitInterference :: Int -> G.Gr R.Statement S.EdgeType -> G.Gr R.Statement S.EdgeType
limitInterference nRegs graph = lim 0 graph
    where
    lim spillCount g = case find ((> nRegs) . Set.size . _lOut . snd) $ G.labNodes (liveness g) of
        Nothing -> g
        Just (_, label) -> lim (succ spillCount) $ unliveness $
            case Set.toList $ (label ^. lOut) `Set.difference` (maybeToSet (label ^. lDef)) of
                [] -> error "no room"
                (r : _) -> spillReg spillCount r (liveness g)

spillReg :: Int -> R.Register -> G.Gr LiveLabel S.EdgeType -> G.Gr LiveLabel S.EdgeType
spillReg nextStackIndex r g = flip execState g $ do
    mapM_ (modifyGDecomp loadReg) $ filterBy _lUse
    mapM_ (modifyGDecomp storeReg) $ filterBy (maybeToSet . _lDef)
    where
    filterBy f = filter (\n -> r `Set.member` (f $ fromJust $ G.lab g n)) (G.nodes g)

    modifyGDecomp f n = do
        g <- get
        case G.match n g of
            (Nothing, _) -> error "match failure"
            (Just c, g') -> f (c, g')

    loadReg ((ins, n, LiveLabel st def uses rIns rOuts, outs), g') = do
        g <- get
        let allRegs = [def | (_, LiveLabel { _lDef = Just def }) <- G.labNodes g']
            r' = head $ Set.toList $ Set.fromList allRegs `Set.difference` (Set.delete r rIns)
            load = ( ins
                   , head (G.newNodes 1 g')
                   , LiveLabel (R.Load nextStackIndex r') (Just r') uses rIns rOuts
                   , []
                   )
            newLabel = LiveLabel (R.mapRegs (\x -> if x == r then r' else x) st) def uses rIns rOuts

        put (([(S.Step, G.node' load)], n, newLabel, outs) G.& (load G.& g'))

    storeReg ((ins, n, label@(LiveLabel st def uses rIns rOuts), outs), g') = do
        g <- get
        let store = ( [(S.Step, n)]
                    , head (G.newNodes 1 g)
                    , lSt .~ R.Store r nextStackIndex $ label
                    , outs
                    )

        put (store G.& ((ins, n, label, []) G.& g'))

squashRegs :: Int -> G.Gr R.Statement S.EdgeType -> G.Gr R.Statement S.EdgeType
squashRegs nRegs g = G.nmap (R.mapRegs (regMap M.!)) g
    where
    regMap = makeRegMap nRegs (interference g)

interference :: G.Gr R.Statement S.EdgeType -> G.Gr () ()
interference g = G.mkUGraph (Set.toList $ concatSet groups) (Set.toList edgeSet)
    where
    groups = Set.fromList $ map (_lOut . snd) $ G.labNodes (liveness g)
    edgesInGroup group = Set.fromList [ (from, to)
                                      | from <- Set.toList group
                                      , to <- Set.toList $ Set.delete from group]
    edgeSet = concatSet $ Set.map edgesInGroup groups

makeRegMap :: Int -> G.Gr () () -> M.Map R.Register R.Register
makeRegMap nRegs g = M.fromList $ evalState (whileM ((not . G.isEmpty) <$> get) select) initialGraph
    where
    initialGraph = G.nmap (const $ Set.fromList [0 .. nRegs - 1]) g :: G.Gr (Set.Set R.Register) ()
    select = do
        g <- get
        let ((_, n, pool, outs), g') = G.matchAny g
            r' = head $ Set.toList pool
            takeR' n' pool' = if n' `elem` map snd outs then Set.delete r' pool' else pool'
        put $ G.gmap (\(ins', n', pool', outs') -> (ins', n', takeR' n' pool', outs')) g'
        return (n, r')

liveness :: G.Gr R.Statement S.EdgeType -> G.Gr LiveLabel S.EdgeType
liveness graph = until (\g' -> g' == makePass g') makePass initialGraph
    where
    initialGraph = G.nmap (\s -> LiveLabel s (R.def s) (R.uses s) Set.empty Set.empty) graph
    linearNodes = map fst $ linear graph
    makePass g = foldr update g linearNodes
        where
        update n g = case G.match n g of
            (Just (ins, _, LiveLabel s def uses rIns rOuts, outs), g') ->
                let rIns' = uses `Set.union` (rOuts `Set.difference` (maybeToSet def))
                    succRIns = map (_lIn . fromJust . G.lab g) (map snd outs)
                    rOuts' = (maybeToSet def) `Set.union` (foldr Set.union Set.empty succRIns)
                    s' = LiveLabel s def uses rIns' rOuts'
                in (ins, n, s', outs) G.& g'
            (Nothing, _) -> error "match failure"

unliveness :: G.Gr LiveLabel S.EdgeType -> G.Gr R.Statement S.EdgeType
unliveness g = G.nmap _lSt g

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

maybeToSet :: Maybe Int -> Set.Set Int
maybeToSet = Set.fromList . maybeToList

concatSet :: Ord a => Set.Set (Set.Set a) -> Set.Set a
concatSet = Set.foldr Set.union Set.empty

modifyContext :: (G.Context a b -> G.Context a b) -> G.Gr a b -> G.Node -> G.Gr a b
modifyContext f g n = G.gmap (\c@(_, n', _, _) -> if n' == n then f c else c) g
