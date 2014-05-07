{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import qualified AST as U
import qualified ASTTyped as T
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Graph
import Control.Lens
import Control.Applicative

typeCheck :: U.Program -> T.Program
typeCheck program = if validClassHierarchy (program ^. U.pClasses)
    then typeCheckProgram program
    else error "Invalid class hierarchy"

typeCheckProgram :: U.Program -> T.Program
typeCheckProgram program = T.Program main' classes'
    where
    main' = typeCheckClass program (program ^. U.pMain)
    classes' = map (typeCheckClass program) (program ^. U.pClasses)

typeCheckClass :: U.Program -> U.Class -> T.Class
typeCheckClass p c@(U.Class name parent fields methods)
    | length (nub $ map (^. U.mName) methods) == length methods =
        T.Class name parent fields (map (typeCheckMethod p c) methods)
    | otherwise =  error "Duplicate method"

typeCheckMethod :: U.Program -> U.Class -> U.Method -> T.Method
typeCheckMethod p c m@(U.Method retType name ps ls ss ret)
    | retType == snd (typeCheckExpression p c m ret) =
        let tcS_ = fst . typeCheckStatement p c m
            tcE_ = fst . typeCheckExpression p c m
        in T.Method retType name ps ls (map tcS_ ss) (tcE_ ret)
    | otherwise =  error "Return type of method must match declaration"

typeCheckStatement :: U.Program -> U.Class -> U.Method -> U.Statement -> (T.Statement, U.Type)
typeCheckStatement p c m s = (s', U.TypeVoid)
    where
    s' = case s of
        U.Block ss -> T.Block $ map tcS_ ss
        U.If condition true falseMaybe -> case tcE condition of
            (e, U.TypeBoolean) -> T.If e (tcS_ true) (tcS_ <$> falseMaybe)
            _ -> error "Type of if condition must be boolean"
        U.While condition body -> case tcE condition of
            (e, U.TypeBoolean) -> T.While e (tcS_ body)
            _ -> error "Type of while condition must be boolean"
        U.Print expression -> case tcE expression of
            (e, U.TypeInt) -> T.Print e
            _ -> error "Type of print must be int"
        U.ExpressionStatement expression -> T.ExpressionStatement (tcE_ expression)
    tcS = typeCheckStatement p c m
    tcE = typeCheckExpression p c m
    tcS_ = fst . tcS
    tcE_ = fst . tcE

typeCheckExpression :: U.Program -> U.Class -> U.Method -> U.Expression -> (T.Expression, U.Type)
typeCheckExpression p c m e = case e of
    U.LiteralInt value -> (T.LiteralInt value, U.TypeInt)
    U.LiteralBoolean value -> (T.LiteralBoolean value, U.TypeBoolean)
    U.Assignment target value ->
        let (t', t'Type) = tcE target
            (v', v'Type) = tcE value
            e' = case t' of
                T.VariableGet name -> T.VariableAssignment name v'
                T.MemberGet cName object fName -> T.MemberAssignment cName object fName v'
                T.IndexGet array index -> T.IndexAssignment array index v'
                _ -> error "Invalid target of assignment"
        in if v'Type `subtype` t'Type
            then (e', t'Type)
            else error "Assignment value must be a subtype"
    U.Binary l op r ->
        let (l', l't) = tcE l
            (r', r't) = tcE r
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
    U.Not e -> case tcE e of
        (e', U.TypeBoolean) -> (T.Not e', U.TypeBoolean)
        _ -> error "Not operand must be boolean"
    U.IndexGet array index ->
        let (array', array't) = tcE array
            (index', index't) = tcE index
        in if array't == U.TypeIntArray && index't == U.TypeInt
            then (T.IndexGet array' index', U.TypeInt)
            else error "Array must be int[] and index must be int"
    U.Call object method args ->
        let (object', object't) = tcE object
            args' = map tcE args
        in case object't of
            U.TypeObject cName -> case findClassMethod (M.lookup cName classMap) method of
                Just (implementor, m) ->
                    let ps = m ^. U.mParameters
                        argCountCorrect = length args == length ps
                        argTypesCorrect = and $ zipWith subtype (map snd args') (map (^. U.vType) ps)
                    in if argCountCorrect && argTypesCorrect
                        then (T.Call (implementor ^. U.cName) object' method (map tcE_ args), m ^. U.mReturnType)
                        else error "Number and types of arguments to method call must match definition"
                Nothing -> error "Method not found"
            _ -> error "Method call must be performed on an object"
    U.MemberGet object fName ->
        let (object', object't) = tcE object
        in case object't of
            U.TypeObject cName -> case findClassField (M.lookup cName classMap) fName of
                Just (c, field) -> (T.MemberGet (c ^. U.cName) object' fName, field ^. U.vType)
                Nothing -> error "Field not found"
            U.TypeIntArray -> if fName == "length"
                then (T.IntArrayLength object', U.TypeInt)
                else error "Int arrays only have a length field"
            _ -> error "Member access must be performed on an object or length of array"
    U.VariableGet name -> case find (\v -> v ^. U.vName == name) (m ^. U.mLocals ++ m ^. U.mParameters) of
        Just v -> (T.VariableGet name, v ^. U.vType)
        Nothing -> tcE (U.MemberGet U.This name)
    U.This -> (T.This, U.TypeObject (c ^. U.cName))
    U.NewIntArray size -> case tcE size of
        (size', U.TypeInt) -> (T.NewIntArray size', U.TypeIntArray)
        _ -> error "Size of new int array must be int"
    U.NewObject cName -> if M.member cName classMap
        then (T.NewObject cName, U.TypeObject cName)
        else error "Class not found"
    where
    tcE = typeCheckExpression p c m
    tcE_ = fst . tcE
    findClassField Nothing _ = Nothing
    findClassField (Just c) fName = case find (\f -> f ^. U.vName == fName) (c ^. U.cFields) of
        Just f -> Just (c, f)
        Nothing -> findClassField (M.lookup (c ^. U.cParent) classMap) fName
    findClassMethod Nothing _ = Nothing
    findClassMethod (Just c) mName = case find (\m -> m ^. U.mName == mName) (c ^. U.cMethods) of
        Just m -> Just (c, m)
        Nothing -> findClassMethod (M.lookup (c ^. U.cParent) classMap) mName
    classMap = M.fromList $ map (\c -> (c ^. U.cName, c)) (p ^. U.pClasses)
    subtype (U.TypeObject name) b@(U.TypeObject name') = if name == name'
        then True
        else case M.lookup name classMap of
            Just childClass -> subtype (U.TypeObject (childClass ^. U.cParent)) b
            Nothing -> False
    subtype a b = a == b

validClassHierarchy :: [U.Class] -> Bool
validClassHierarchy classes = allUnique names && all (pathTo "Object") names
    where
    allUnique as = length (nub as) == length as
    names = "Object" : map (^. U.cName) classes
    pathTo to from = fromMaybe False $ path graph <$> vertex from <*> vertex to
        where
        graph = buildG (0, length names - 1) edges
        edges = mapMaybe edgeMaybe classes
        edgeMaybe c = (,) <$> vertex (c ^. U.cName) <*> vertex (c ^. U.cParent)
        vertex = flip M.lookup m
        m = M.fromList $ zip names [0 .. ]
