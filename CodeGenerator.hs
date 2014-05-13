module CodeGenerator where

import qualified ASTTyped as T
import qualified AST as U
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

bug x = trace (ppShow x) x
bug' x y = trace (ppShow x) y

generate :: T.Program -> R.Program -> String
generate ast (R.Program (R.Class _ _ [m]) cs) = unlines $ execWriter $ do
    boilerplate
    line "mj_main:"
    gMethod ast m
    mapM_ (gClass ast) cs

gClass :: T.Program -> R.Class -> Writer [String] ()
gClass ast (R.Class name fs ms) = do
    line ".data"
    line $ printf ".align %s" (show wordsize)
    line ""

    line $ printf "mj__v_%s:" name
    mapM_ (\(R.Method mName _) -> line $ printf " .word mj__m_%s_%s" (implementor ast name mName) mName) ms

    line ".text"
    mapM_ (\m@(R.Method mName _) -> line (printf "mj__m_%s_%s:" name mName) >> gMethod ast m) ms

gMethod :: T.Program -> R.Method -> Writer [String] ()
gMethod ast (R.Method name g) = do
    line $ " add $sp, $sp, " ++ show (-1 * wordsize)
    line $ " sw $fp, ($sp)"
    line $ " move $fp, $sp"

    let spillSpace = case maximumMay [o | (R.Store _ o) <- map snd (G.labNodes g)] of
        { Nothing -> 0
        ; Just o -> o + 1
        }
    let fromSaved list = [fr | [r] <- map (Set.toList . R.def . snd) (G.labNodes g)
                          , let fr = freeRegisters !! r
                          , fr `elem` list]
    let calleeSaved = fromSaved calleeSavedRegisters
    let callerSaved = nub $ 2 : 3 : fromSaved callerSavedRegisters

    line $ " add $sp, $sp, " ++ show (-(spillSpace + length callerSaved + 1) * wordsize) -- +1 for fp

    forM_ calleeSaved $ \r -> do
        line $ " add $sp, $sp, " ++ show wordsize
        line $ printf " sw $%s, ($sp)" (show $ registers !! r)

    line $ " add $sp, $sp, " ++ show (-wordsize)
    line " sw $ra, ($sp)"

    let argSpace = case maximumMay [p | (R.Arg _ p) <- map snd (G.labNodes g)] of
        { Nothing -> 0
        ; Just p -> p + 1
        }

    line $ " add $sp, $sp, " ++ show (argSpace * (-wordsize))

    mapM_ (gStatement ast name spillSpace callerSaved) (map (G.context g) (G.nodes g))

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

gStatement :: T.Program -> String -> Int -> [R.Register] -> G.Context R.Statement S.EdgeType -> Writer [String] ()
gStatement ast mName spillSpace callerSaved (ins, node, statement, outs) = do
    line $ " # " ++ show statement
    case statement of
        R.Load offset r            -> line $ printf " lw $%s, %s($fp)" (reg r) (show $ (offset + 1) * (-wordsize))
        R.Null t r                 -> line $ printf " move $%s, $zero" (reg r)
        R.NewObj s1 r              -> do
            storeAll (callerSaved \\ [freeRegisters !! r]) spillSpace
            line $ printf " la $a0, mj__v_%s" s1
            line $ printf " li $a1, %s" (show $ objectSize ast s1)
            line " jal minijavaNew"
            line $ printf " move $%s, $v0" (reg r)
            loadAll (callerSaved \\ [freeRegisters !! r]) spillSpace
        R.NewIntArray r1 r         -> do
            storeAll (callerSaved \\ [freeRegisters !! r]) spillSpace
            line $ printf " move $a0, $%s" (reg r1)
            line " jal minijavaNewArray"
            line $ printf " move $%s, $v0" (reg r)
            loadAll (callerSaved \\ [freeRegisters !! r]) spillSpace
        R.This r                   -> line $ printf " move $%s, $v0" (reg r)
        R.SInt v r                 -> line $ printf " li $%s, %s" (reg r) (show v)
        R.SBoolean v r             -> line $ printf " li $%s, %s" (reg r) (if v then "1" else "0")
        R.Parameter position r     -> line $ printf " lw $%s, %s($fp)" (reg r) (show $ (position + 1) * wordsize)
        R.Call s1 r1 s2 is r       -> do
            storeAll (callerSaved \\ [freeRegisters !! r]) spillSpace
            line $ printf " move $v0, $%s" (reg r1)
            line $ printf " lw $v1, ($v0)"
            line $ printf " lw $v1, %s($v1)" (show $ methodOffset ast s1 s2 * wordsize)
            line " jal $v1"
            line $ printf " move $%s, $v0" (reg r)
            loadAll (callerSaved \\ [freeRegisters !! r]) spillSpace
        R.MemberGet s1 r1 s2 r     -> line $ printf " lw $%s, %s($%s)" (reg r) (fieldOffset ast s1 s2) (reg r1)
        R.MemberAssg s1 r1 s2 r2 r -> line "MemberAssg not implemented"
        R.VarAssg r1 r             -> line $ printf " move $%s, $%s" (reg r) (reg r1)
        R.IndexGet r1 r2 r         -> line "IndexGet not implemented"
        R.IndexAssg r1 r2 r3 r     -> line "IndexAssg not implemented"
        R.Not r1 r                 -> line $ printf " seq $%s, $zero, $%s" (reg r) (reg r1)
        R.Lt r1 r2 r               -> line $ printf " slt $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Le r1 r2 r               -> line $ printf " sle $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Eq r1 r2 r               -> line $ printf " seq $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Ne r1 r2 r               -> line $ printf " sne $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Gt r1 r2 r               -> line $ printf " sgt $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Ge r1 r2 r               -> line $ printf " sge $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.And r1 r2 r              -> do
            line $ printf " add $%s, $%s, $%s" (reg r2) (reg r1) (reg r2)
            line $ printf " seq $%s, $%s, %s" (reg r) (reg r2) "2"
        R.Or r1 r2 r               -> do
            line $ printf " add $%s, $%s, $%s" (reg r2) (reg r1) (reg r2)
            line $ printf " sgt $%s, $%s, %s" (reg r) (reg r2) "0"
        R.Plus r1 r2 r             -> line $ printf " add $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Minus r1 r2 r            -> line $ printf " sub $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Mul r1 r2 r              -> line $ printf " mul $%s, $%s, $%s" (reg r) (reg r1) (reg r2)
        R.Div r1 r2 r              -> do
            line $ printf " div $%s, $%s" (reg r1) (reg r2)
            line $ printf " mflo $%s" (reg r)
        R.Mod r1 r2 r              -> do
            line $ printf " div $%s, $%s" (reg r1) (reg r2)
            line $ printf " mfhi $%s" (reg r)
        R.Store r1 offset          -> line $ printf " sw $%s, %s($fp)" (reg r1) (show $ (offset + 1) * (-wordsize))
        R.Branch r1                -> line $ printf " bne $%s, $zero, .l_%s" (reg r1) (show $ head $ [n | (S.Jump, n) <- outs])
        R.NBranch r1               -> line $ printf " beq $%s, $zero, .l_%s" (reg r1) (show $ head $ [n | (S.Jump, n) <- outs])
        R.Arg r1 p                 -> line $ printf " sw $%s, %s($sp)" (reg r1) (show $ p)
        R.Return r1                -> do
            line $ printf " move $v0, $%s" (reg r1)
            line $ printf " j .ret_%s" mName
        R.Print r1                 -> do
            storeAll callerSaved spillSpace
            line $ printf " move $a0, $%s" (reg r1)
            line " jal minijavaPrint"
            loadAll callerSaved spillSpace
        R.BeginMethod              -> return ()
        R.Label                    -> line $ printf " .l_%s:" (show node)
        R.Goto                     -> line $ printf " j .l_%s" (show $ snd $ head outs)

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

calleeSavedRegisters :: [R.Register]
calleeSavedRegisters =
    [ 16, 17, 18, 19, 20, 21, 22, 23 -- s*
    , 28 -- gp
    , 31 -- ra
    ]

callerSavedRegisters :: [R.Register]
callerSavedRegisters =
    [ 2, 3 -- v*
    , 4, 5, 6, 7 -- a*
    , 8, 9, 10, 11, 12, 13, 14, 15, 24, 25 -- t*
    ]

freeRegisters :: [R.Register]
freeRegisters =
    [ 4, 5, 6, 7 -- a*
    , 8, 9, 10, 11, 12, 13, 14, 15 -- t*
    , 16, 17, 18, 19, 20, 21, 22, 23 -- s*
    , 24, 25 -- t*
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

reg :: R.Register -> String
reg r = registers !! (freeRegisters !! r)

storeAll :: [R.Register] -> Int -> Writer [String] ()
storeAll regs spillSpace = zipWithM_ (\r i -> line $ printf " sw $%s, %s($fp)" (registers !! r) (show $ (spillSpace + i + 1) * (-wordsize))) regs [0 .. ]

loadAll :: [R.Register] -> Int -> Writer [String] ()
loadAll regs spillSpace = zipWithM_ (\r i -> line $ printf " lw $%s, %s($fp)" (registers !! r) (show $ (spillSpace + i + 1) * (-wordsize))) regs [0 .. ]

objectSize :: T.Program -> String -> Int
objectSize _ "Object" = 1
objectSize ast@(T.Program m cs) cName = case find ((== cName) . T._cName) cs of
    Nothing -> error "no class found"
    Just (T.Class _ parent fs _) -> length fs + objectSize ast parent

methodOffset :: T.Program -> String -> String -> Int
methodOffset ast@(T.Program m cs) cName mName = case find ((== cName) . T._cName) cs of
    Nothing -> error "no class found"
    Just (T.Class _ parent _ ms) -> fromJust $ lookup mName $ reverse $ zip ((allMethods ast parent) ++ (map T._mName ms)) [0 .. ]

allMethods :: T.Program -> String -> [String]
allMethods ast@(T.Program m cs) cName = case find ((== cName) . T._cName) cs of
    Nothing -> []
    Just (T.Class _ parent _ ms) -> allMethods ast parent ++ (map T._mName ms)

fieldOffset :: T.Program -> String -> String -> Int
fieldOffset ast@(T.Program m cs) cName fName = case find ((== cName) . T._cName) cs of
    Nothing -> error "no class found"
    Just (T.Class _ parent fs _) -> fromJust $ lookup fName $ reverse $ zip ((allFields ast parent) ++ (map U._vName fs)) [0 .. ]

allFields :: T.Program -> String -> [String]
allFields ast@(T.Program m cs) cName = case find ((== cName) . T._cName) cs of
    Nothing -> []
    Just (T.Class _ parent fs _) -> allFields ast parent ++ (map U._vName fs)
