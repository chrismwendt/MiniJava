module CodeGenerator where

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
import Text.Printf

generate :: T.Program -> R.Program -> String
generate ast (R.Program (R.Class _ _ [m]) cs) = unlines $ execWriter $ do
    boilerplate
    line "mj_main:"
    gMethod m
    mapM_ (gClass ast) cs

gClass :: T.Program -> R.Class -> Writer [String] ()
gClass ast (R.Class name fs ms) = do
    line ".data"
    line $ printf ".align %s" (show wordsize)
    line ""

    line $ printf "mj__v_%s:" name
    mapM_ (\(R.Method mName _) -> line $ printf " .word mj__m_%s_%s" (implementor ast name mName) mName) ms

    line ".text"
    mapM_ (\m@(R.Method mName _) -> line (printf "mj__m_%s_%s:" name mName) >> gMethod m) ms

gMethod :: R.Method -> Writer [String] ()
gMethod (R.Method name g) = do
    line $ " add $sp, $sp, " ++ show (-1 * wordsize)
    line $ " sw $fp, ($sp)"
    line $ " move $fp, $sp"

    let maxOffset = foldr max 0 [o | (R.Store _ o) <- map snd (G.labNodes g)]
    let spillSpace = maxOffset + 1
    let calleeSaved = filter (\r -> any (r `Set.member`) (map (R.def . snd) $ G.labNodes g)) $ Set.toList calleeSavedRegisters
    let callerSaved = filter (\r -> any (r `Set.member`) (map (R.def . snd) $ G.labNodes g)) $ Set.toList callerSavedRegisters

    line $ " add $sp, $sp, " ++ show (-(spillSpace + length callerSaved + 1) * wordsize) -- +1 for fp

    forM_ calleeSaved $ \r -> do
        line $ " add $sp, $sp, " ++ show wordsize
        line $ printf " sw $%s, ($sp)" (show $ registers !! r)

    line $ " add $sp, $sp " ++ show (-wordsize)
    line " sw $ra, ($sp)"

    let maxArgPosition = foldr max 0 [p | (R.Arg _ p) <- map snd (G.labNodes g)]
    let argSpace = maxArgPosition + 1

    line $ " add $sp, $sp, " ++ show (argSpace * (-wordsize))

    mapM_ (gStatement spillSpace callerSaved) (map (G.context g) (G.nodes g))

    line $ printf " .ret_%s:" name

    line $ " add $sp, $sp, " ++ show (argSpace * wordsize)
    line " lw $ra, ($sp)"
    line $ " add $sp, $sp, " ++ show wordsize

    forM_ (reverse calleeSaved) $ \r -> do
        line $ printf " lw $%s, ($sp)" (show $ registers !! r)
        line $ " add $sp, $sp, " ++ show wordsize

    line " move $sp, $fp"
    line " lw $fp, ($sp)"
    line $ " add $sp, $sp, " ++ show wordsize
    line " j $ra"

gStatement :: Int -> [R.Register] -> G.Context R.Statement S.EdgeType -> Writer [String] ()
gStatement spillSpace callerSaved (ins, node, statement, outs) = do
    line $ " # " ++ show statement
    case statement of
        R.Load offset r            -> line "Load not implemented"
        R.Null t r                 -> line "Null not implemented"
        R.NewObj s1 r              -> line "NewObj not implemented"
        R.NewIntArray r1 r         -> line "NewIntArray not implemented"
        R.This r                   -> line "This not implemented"
        R.SInt v r                 -> line "SInt not implemented"
        R.SBoolean v r             -> line "SBoolean not implemented"
        R.Parameter position r     -> line "Parameter not implemented"
        R.Call s1 r1 s2 is r       -> line "Call not implemented"
        R.MemberGet s1 r1 s2 r     -> line "MemberGet not implemented"
        R.MemberAssg s1 r1 s2 r2 r -> line "MemberAssg not implemented"
        R.VarAssg r1 r             -> line "VarAssg not implemented"
        R.IndexGet r1 r2 r         -> line "IndexGet not implemented"
        R.IndexAssg r1 r2 r3 r     -> line "IndexAssg not implemented"
        R.Not r1 r                 -> line "Not not implemented"
        R.Lt r1 r2 r               -> line "Lt not implemented"
        R.Le r1 r2 r               -> line "Le not implemented"
        R.Eq r1 r2 r               -> line "Eq not implemented"
        R.Ne r1 r2 r               -> line "Ne not implemented"
        R.Gt r1 r2 r               -> line "Gt not implemented"
        R.Ge r1 r2 r               -> line "Ge not implemented"
        R.And r1 r2 r              -> line "And not implemented"
        R.Or r1 r2 r               -> line "Or not implemented"
        R.Plus r1 r2 r             -> line "Plus not implemented"
        R.Minus r1 r2 r            -> line "Minus not implemented"
        R.Mul r1 r2 r              -> line "Mul not implemented"
        R.Div r1 r2 r              -> line "Div not implemented"
        R.Mod r1 r2 r              -> line "Mod not implemented"
        R.Store r1 offset          -> line "Store not implemented"
        R.Branch r1                -> line "Branch not implemented"
        R.NBranch r1               -> line "NBranch not implemented"
        R.Arg r1 p                 -> line "Arg not implemented"
        R.Return r1                -> line "Return not implemented"
        R.Print r1                 -> line "Print not implemented"
        R.BeginMethod              -> line "BeginMethod not implemented"
        R.Label                    -> line "Label not implemented"
        R.Goto                     -> line "Goto not implemented"

boilerplate :: Writer [String] ()
boilerplate = do
    line "main:"
    line " jal mj_main"
    line " li $v0, 10"
    line " syscall"
    line ""

    line "minijavaNew:"
    line " move $t0, $a0"
    line " mul $a0, $a1, 4"
    line " li $v0, 9"
    line " syscall"
    line " sw $t0, ($v0)"
    line " j $ra"
    line ""

    line "minijavaNewArray:"
    line " move $t0, $a0"
    line " mul $a0, $a0, 4"
    line " add $a0, $a0, 4"
    line " li $v0, 9"
    line " syscall"
    line " sw $t0, ($v0)"
    line " j $ra"
    line ""

    line ".data"
    line ".align 4"
    line "minijavaNewline:"
    line " .asciiz \"\\n\""
    line ""

    line ".text"
    line "minijavaPrint:"
    line " li $v0, 1"
    line " syscall"
    line " la $a0, minijavaNewline"
    line " li $v0, 4"
    line " syscall"
    line " j $ra"
    line ""

wordsize :: Int
wordsize = 4

implementor :: T.Program -> String -> String -> String
implementor ast@(T.Program m cs) cName mName = case find ((== cName) . T._cName) cs of
    Nothing -> error "no class found"
    Just (T.Class cName' parent _ ms) -> case find (== mName) (map T._mName ms) of
        Nothing -> implementor ast parent mName
        Just _ -> cName'

line :: String -> Writer [String] ()
line s = tell [s]

calleeSavedRegisters :: Set.Set R.Register
calleeSavedRegisters = Set.fromList
    [ 16, 17, 18, 19, 20, 21, 22, 23 -- s*
    , 28 -- gp
    , 31 -- ra
    ]

callerSavedRegisters :: Set.Set R.Register
callerSavedRegisters = Set.fromList
    [ 2, 3 -- v*
    , 4, 5, 6, 7 -- a*
    , 8, 9, 10, 11, 12, 13, 14, 15, 24, 25 -- t*
    ]

registers :: [String]
registers =
    [ "zero"
    , "at"
    , "v0", "v1"
    , "a0", "a1", "a2", "a3"
    , "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7"
    , "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7"
    , "t8", "t9"
    , "k0", "k1"
    , "gp", "sp", "fp", "ra"
    ]
