{-# LANGUAGE TemplateHaskell #-}

module TypeCheckerAST where

import qualified AST
import qualified ASTUntyped as U
import qualified ASTTyped as T
import qualified Data.Map as M
import Safe
import Data.Maybe
import Data.List
import Data.Graph
import Control.Lens
import Data.Function
import Control.Applicative
import Control.Monad.Reader
import Debug.Trace

typeCheck :: U.Program -> T.Program
typeCheck program = if validClassHierarchy (program ^. U.pClasses)
    then typeCheckProgram program
    else error "Invalid class hierarchy"

typeCheckProgram :: U.Program -> T.Program
typeCheckProgram program = T.Program main' classes'
    where
    (main', _) = typeCheckStatement program pseudoClass pseudoMethod (program ^. U.pMain)
    classes' = map (typeCheckClass program) (program ^. U.pClasses)
    pseudoClass = U.Class
        { U._cName = ""
        , U._cParent = ""
        , U._cFields = []
        , U._cMethods = []
        }
    pseudoMethod = U.Method
        { U._mReturnType = AST.TypeObject ""
        , U._mName = ""
        , U._mParameters = []
        , U._mLocals = []
        , U._mStatements = []
        , U._mReturn = U.This
        }

typeCheckClass :: U.Program -> U.Class -> T.Class
typeCheckClass p c
    | length (nub $ map (^. U.mName) (c ^. U.cMethods)) == length (c ^. U.cMethods) = T.Class
        { T._cName = c ^. U.cName
        , T._cParent = c ^. U.cParent
        , T._cFields = c ^. U.cFields
        , T._cMethods = map (typeCheckMethod p c) (c ^. U.cMethods)
        }
    | otherwise =  error "Duplicate method"

typeCheckMethod :: U.Program -> U.Class -> U.Method -> T.Method
typeCheckMethod p c m
    | m ^. U.mReturnType == snd (typeCheckExpression p c m (m ^. U.mReturn)) = T.Method
        { T._mReturnType = m ^. U.mReturnType
        , T._mName = m ^. U.mName
        , T._mParameters = m ^. U.mParameters
        , T._mLocals = m ^. U.mLocals

        , T._mStatements = map (fst . typeCheckStatement p c m) (m ^. U.mStatements)
        , T._mReturn = (fst . typeCheckExpression p c m) (m ^. U.mReturn)
        }
    | otherwise =  error "Return type of method must match declaration"

typeCheckStatement :: U.Program -> U.Class -> U.Method -> U.Statement -> (T.Statement, AST.Type)
typeCheckStatement p c m s = (s', AST.TypeVoid)
    where
    s' = case s of
        U.Block ss -> T.Block $ map tcS_ ss
        U.If condition true falseMaybe -> case tcE condition of
            (e, AST.TypeBoolean) -> T.If e (tcS_ true) (tcS_ <$> falseMaybe)
            _ -> error "Type of if condition must be boolean"
        U.While condition body -> case tcE condition of
            (e, AST.TypeBoolean) -> T.While e (tcS_ body)
            _ -> error "Type of while condition must be boolean"
        U.Print expression -> case tcE expression of
            (e, AST.TypeInt) -> T.Print e
            _ -> error "Type of print must be int"
        U.ExpressionStatement expression -> T.ExpressionStatement (tcE_ expression)
    tcS = typeCheckStatement p c m
    tcE = typeCheckExpression p c m
    tcS_ = fst . tcS
    tcE_ = fst . tcE

typeCheckExpression :: U.Program -> U.Class -> U.Method -> U.Expression -> (T.Expression, AST.Type)
typeCheckExpression p c m e = case e of
    U.LiteralInt value -> (T.LiteralInt value, AST.TypeInt)
    U.LiteralBoolean value -> (T.LiteralBoolean value, AST.TypeBoolean)
    U.Assignment target value ->
        let (t', t'Type) = tcE target
            (v', v'Type) = tcE value
            e' = case t' of
                T.VariableGet name -> T.VariableAssignment name v'
                T.MemberGet className object field -> T.MemberAssignment className object field v'
                T.IndexGet array index -> T.IndexAssignment array index v'
        in if v'Type `subtype` t'Type
            then (t', t'Type)
            else error "Assignment value must be a subtype"
    U.Binary l op r ->
        let (l', l't) = tcE l
            (r', r't) = tcE r
            e' = T.Binary l' op r'
            checkOp lExpect rExpect resultType = if l't == lExpect && r't == rExpect
                then (e', resultType)
                else error "Incorrect types to binary operator"
            logicOp   = checkOp AST.TypeBoolean AST.TypeBoolean AST.TypeBoolean
            compareOp = checkOp AST.TypeInt     AST.TypeInt     AST.TypeBoolean
            arithOp   = checkOp AST.TypeInt     AST.TypeInt     AST.TypeInt
        in case op of
            AST.Lt    -> compareOp
            AST.Le    -> compareOp
            AST.Eq    -> compareOp
            AST.Ne    -> compareOp
            AST.Gt    -> compareOp
            AST.Ge    -> compareOp
            AST.And   -> logicOp
            AST.Or    -> logicOp
            AST.Plus  -> arithOp
            AST.Minus -> arithOp
            AST.Mul   -> arithOp
            AST.Div   -> arithOp
            AST.Mod   -> arithOp
    U.Not e -> case tcE e of
        (e', AST.TypeBoolean) -> (e', AST.TypeBoolean)
        _ -> error "Not operand must be boolean"
    U.IndexGet array index ->
        let (array', array't) = tcE array
            (index', index't) = tcE index
        in if array't == AST.TypeIntArray && index't == AST.TypeInt
            then (T.IndexGet array' index', AST.TypeInt)
            else error "Array must be int[] and index must be int"
    U.Call object method args ->
        let (object', object't) = tcE object
            args' = map tcE args
        in case object't of
            AST.TypeObject className -> case msum [find ((== className) . (^. U.cName)) (p ^. U.pClasses) >>= find ((== method) . (^. U.mName)) . (^. U.cMethods) >>= (\a -> Just (className, a)) . id, findMethod className method] of
                Just (implementor, m) -> if length args == length (m ^. U.mParameters) && and (zipWith subtype (map snd args') (map (^. U.vType) (m ^. U.mParameters)))
                    then (T.Call implementor object' method (map tcE_ args), m ^. U.mReturnType)
                    else error "Number and types of arguments to method call must match definition"
                Nothing -> error "Method not found"
            _ -> error "Method call must be performed on an object"
    U.MemberGet object field ->
        let (object', object't) = tcE object
        in case object't of
            AST.TypeObject className -> case findField className field of
                Just (className', t) -> (T.MemberGet className' object' field, t)
                Nothing -> error "Field not found"
            AST.TypeIntArray -> if field == "length"
                then (T.IntArrayLength object', AST.TypeInt)
                else error "Int arrays only have a length field"
            _ -> error "Member access must be performed on an object or length of array"
    U.VariableGet name -> case find ((== name) . (^. U.vName)) (m ^. U.mLocals ++ m ^. U.mParameters) of
        Just v -> (T.VariableGet name, v ^. U.vType)
        Nothing -> tcE (U.MemberGet U.This name)
    U.This -> (T.This, AST.TypeObject (c ^. U.cName))
    U.NewIntArray size -> case tcE size of
        (size', AST.TypeInt) -> (T.NewIntArray size', AST.TypeIntArray)
    U.NewObject className -> if className `elem` map (^. U.cName) (p ^. U.pClasses)
        then (T.NewObject className, AST.TypeObject className)
        else error "Class not found"
    where
    tcS = typeCheckStatement p c m
    tcE = typeCheckExpression p c m
    tcS_ = fst . tcS
    tcE_ = fst . tcE
    findMethod thisClass methodName = case find ((== thisClass) . (^. U.cName)) (p ^. U.pClasses) >>= find ((== methodName) . (^. U.mName)) . (^. U.cMethods) of
        Just m -> Just (thisClass, m)
        Nothing -> case parentClassMaybe thisClass of
            Just parentClass -> findMethod (parentClass ^. U.cName) methodName
            Nothing -> Nothing
    findField thisClass field = case find ((== thisClass) . (^. U.cName)) (p ^. U.pClasses) >>= find ((== field) . (^. U.vName)) . (^. U.cFields) of
        Just f -> Just (thisClass, f ^. U.vType)
        Nothing -> case parentClassMaybe thisClass of
            Just parentClass -> findField (parentClass ^. U.cName) field
            Nothing -> Nothing
    subtype (AST.TypeObject name) b@(AST.TypeObject name') = if name == name'
        then True
        else case parentClassMaybe name of
            Just parentClass -> subtype (AST.TypeObject (parentClass ^. U.cName)) b
            Nothing -> False
    subtype a b = a == b
    parentClassMaybe name = do
        thisClass <- find ((== name) . (^. U.cName)) (p ^. U.pClasses)
        find ((== (thisClass ^. U.cParent)) . (^. U.cName)) (p ^. U.pClasses)

validClassHierarchy classes = allUnique names && all (pathTo "Object") names
    where
    allUnique as = length (nub as) == length as
    names = "Object" : map (^. U.cName) classes
    pathTo to from = fromMaybe False $ path graph <$> vertex from <*> vertex to
        where
        graph = buildG (0, length names - 1) edges
        edges = mapMaybe edgeMaybe classes
        edgeMaybe c = do
            child <- vertex $ c ^. U.cName
            parent <- vertex $ c ^. U.cParent
            return (child, parent)
        vertex = flip M.lookup m
        m = M.fromList $ zip names [0 .. ]
