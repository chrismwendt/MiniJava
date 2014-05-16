{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Graph.Inductive as G
import Control.Lens
import Control.Applicative
import qualified AST as U
import qualified ASTTyped as T

typeCheck :: U.Program -> T.Program
typeCheck = typeCheckProgram

typeCheckProgram :: U.Program -> T.Program
typeCheckProgram p@(U.Program main classes)
    | validClassHierarchy classes = T.Program main' classes'
    | otherwise = error "Invalid class hierarchy"
    where
    main' = typeCheckClass p main
    classes' = map (typeCheckClass p) classes

typeCheckClass :: U.Program -> U.Class -> T.Class
typeCheckClass p c@(U.Class name parent fields methods)
    | allUnique $ map U._mName methods = T.Class name parent fields methods'
    | otherwise =  error "Duplicate method"
    where
    methods' = map (typeCheckMethod p c) methods

typeCheckMethod :: U.Program -> U.Class -> U.Method -> T.Method
typeCheckMethod p c m@(U.Method retType name ps ls ss ret)
    | retType == tcExpT ret = T.Method retType name ps ls (map tcSt ss) (tcExpE ret)
    | otherwise =  error "Return type of method must match declaration"
    where
    tcSt = typeCheckStatement p c m
    tcExp = typeCheckExpression p c m
    tcExpE = fst . tcExp
    tcExpT = snd . tcExp

typeCheckStatement :: U.Program -> U.Class -> U.Method -> U.Statement -> T.Statement
typeCheckStatement p c m s = s'
    where
    s' = case s of
        U.Block ss -> T.Block $ map tcSt ss
        U.If condition true falseMaybe -> case tcExpT condition of
            U.TypeBoolean -> T.If (tcExpE condition) (tcSt true) (tcSt <$> falseMaybe)
            _ -> error "Type of if condition must be boolean"
        U.While condition body -> case tcExpT condition of
            U.TypeBoolean -> T.While (tcExpE condition) (tcSt body)
            _ -> error "Type of while condition must be boolean"
        U.Print expression -> case tcExpT expression of
            U.TypeInt -> T.Print (tcExpE expression)
            _ -> error "Type of print must be int"
        U.ExpressionStatement expression -> T.ExpressionStatement (tcExpE expression)
    tcSt = typeCheckStatement p c m
    tcExp = typeCheckExpression p c m
    tcExpE = fst . tcExp
    tcExpT = snd . tcExp

typeCheckExpression :: U.Program -> U.Class -> U.Method -> U.Expression -> (T.Expression, U.Type)
typeCheckExpression p c m e = case e of
    U.LiteralInt value -> (T.LiteralInt value, U.TypeInt)
    U.LiteralBoolean value -> (T.LiteralBoolean value, U.TypeBoolean)
    U.Assignment target value ->
        let (t', t'Type) = tcExp target
            (v', v'Type) = tcExp value
            e' = case t' of
                T.VariableGet name -> T.VariableAssignment name v'
                T.MemberGet cName object fName -> T.MemberAssignment cName object fName v'
                T.IndexGet array index -> T.IndexAssignment array index v'
                _ -> error "Invalid target of assignment"
        in if v'Type `subtype` t'Type
            then (e', t'Type)
            else error "Assignment value must be a subtype"
    U.Binary l op r ->
        let (l', l't) = tcExp l
            (r', r't) = tcExp r
            e' = T.Binary l' op r'
            checkOp lExpect rExpect resultType = if l't == lExpect && r't == rExpect
                then (e', resultType)
                else error "Incorrect types to binary operator"
            logicOp   = checkOp U.TypeBoolean U.TypeBoolean U.TypeBoolean
            compareOp = checkOp U.TypeInt     U.TypeInt     U.TypeBoolean
            arithOp   = checkOp U.TypeInt     U.TypeInt     U.TypeInt
        in case op of
            U.Lt    -> compareOp
            U.Le    -> compareOp
            U.Eq    -> compareOp
            U.Ne    -> compareOp
            U.Gt    -> compareOp
            U.Ge    -> compareOp
            U.And   -> logicOp
            U.Or    -> logicOp
            U.Plus  -> arithOp
            U.Minus -> arithOp
            U.Mul   -> arithOp
            U.Div   -> arithOp
            U.Mod   -> arithOp
    U.Not e -> case tcExp e of
        (e', U.TypeBoolean) -> (T.Not e', U.TypeBoolean)
        _ -> error "Not operand must be boolean"
    U.IndexGet array index ->
        let (array', array't) = tcExp array
            (index', index't) = tcExp index
        in if array't == U.TypeIntArray && index't == U.TypeInt
            then (T.IndexGet array' index', U.TypeInt)
            else error "Array must be int[] and index must be int"
    U.Call object method args ->
        let (object', object't) = tcExp object
            args' = map tcExp args
        in case object't of
            U.TypeObject cName ->
                let (implementor, m) = getClassMethod (M.lookup cName classMap) method
                    ps = m ^. U.mParameters
                    argCountCorrect = length args == length ps
                    argTypesCorrect = and $ zipWith subtype (map snd args') (map U._vType ps)
                in if argCountCorrect && argTypesCorrect
                    then (T.Call (implementor ^. U.cName) object' method (map tcExpE args), m ^. U.mReturnType)
                    else error "Number and types of arguments to method call must match definition"
            _ -> error "Method call must be performed on an object"
    U.MemberGet object fName ->
        let (object', object't) = tcExp object
        in case object't of
            U.TypeObject cName ->
                let (c, field) = getClassField (M.lookup cName classMap) fName
                in (T.MemberGet (c ^. U.cName) object' fName, field ^. U.vType)
            U.TypeIntArray -> if fName == "length"
                then (T.IntArrayLength object', U.TypeInt)
                else error "Int arrays only have a length field"
            _ -> error "Member access must be performed on an object or length of array"
    U.VariableGet name -> case find ((== name) . U._vName) (m ^. U.mLocals ++ m ^. U.mParameters) of
        Just v -> (T.VariableGet name, v ^. U.vType)
        Nothing -> tcExp (U.MemberGet U.This name)
    U.This -> (T.This, U.TypeObject (c ^. U.cName))
    U.NewIntArray size -> case tcExp size of
        (size', U.TypeInt) -> (T.NewIntArray size', U.TypeIntArray)
        _ -> error "Size of new int array must be int"
    U.NewObject cName -> if M.member cName classMap
        then (T.NewObject cName, U.TypeObject cName)
        else error "Class not found"

    where

    classMap = M.fromList $ map (\c -> (c ^. U.cName, c)) (p ^. U.pClasses)

    tcExp = typeCheckExpression p c m
    tcExpE = fst . tcExp
    tcExpT = snd . tcExp

    getClassField Nothing _ = error "Class not found"
    getClassField (Just c) fName = case find (\f -> f ^. U.vName == fName) (c ^. U.cFields) of
        Just f -> (c, f)
        Nothing -> getClassField (M.lookup (c ^. U.cParent) classMap) fName

    getClassMethod Nothing _ = error "Method not found"
    getClassMethod (Just c) mName = case find (\m -> m ^. U.mName == mName) (c ^. U.cMethods) of
        Just m -> (c, m)
        Nothing -> getClassMethod (M.lookup (c ^. U.cParent) classMap) mName

    (U.TypeObject nameA) `subtype` b@(U.TypeObject nameB) = if nameA == nameB
        then True
        else case M.lookup nameA classMap of
            Just (U.Class _ parent _ _) -> U.TypeObject parent `subtype` b
            Nothing -> False
    a `subtype` b = a == b

validClassHierarchy :: [U.Class] -> Bool
validClassHierarchy classes = allUnique names && all (`extends` objectNode) classNodes
    where
    c1 `extends` c2 = c2 `elem` G.reachable c1 classGraph
    classGraph = G.mkUGraph classNodes (mapMaybe edgeMaybe classes) :: G.Gr () ()
    edgeMaybe (U.Class name parent _ _) = do
        n1 <- lookupClass name
        n2 <- lookupClass parent
        return (n1, n2)
    lookupClass name = name `lookup` (zip names classNodes)
    classNodes@(objectNode : _) = zipWith const [0 .. ] names
    names = "Object" : map U._cName classes

allUnique :: Eq a => [a] -> Bool
allUnique as = length (nub as) == length as
