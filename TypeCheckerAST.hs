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
        , U._cParent = Nothing
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
typeCheckClass p c = if length (nub $ map (^. U.mName) (c ^. U.cMethods)) == length (c ^. U.cMethods)
    then T.Class
        { T._cName = c ^. U.cName
        , T._cParent = fromMaybe "Object" $ c ^. U.cParent
        , T._cFields = c ^. U.cFields
        , T._cMethods = map (typeCheckMethod p c) (c ^. U.cMethods)
        }
    else error "Duplicate method"

typeCheckMethod :: U.Program -> U.Class -> U.Method -> T.Method
typeCheckMethod program c method = if method ^. U.mReturnType == snd (typeCheckExpression program c method (method ^. U.mReturn))
    then T.Method
        { T._mReturnType = method ^. U.mReturnType
        , T._mName = method ^. U.mName
        , T._mParameters = method ^. U.mParameters
        , T._mLocals = method ^. U.mLocals

        , T._mStatements = map (fst . typeCheckStatement program c method) (method ^. U.mStatements)
        , T._mReturn = (fst . typeCheckExpression program c method) (method ^. U.mReturn)
        }
    else error "Return type of method must match declaration"

typeCheckStatement :: U.Program -> U.Class -> U.Method -> U.Statement -> (T.Statement, AST.Type)
typeCheckStatement program c method statement = (t, AST.TypeVoid)
    where
    t = case statement of
        U.Block ss -> T.Block $ map st_ ss
        U.If condition true falseMaybe -> case ex condition of
            (e, AST.TypeBoolean) -> T.If e (st_ true) (st_ <$> falseMaybe)
            _ -> error "Type of if condition must be boolean"
        U.While condition body -> case ex condition of
            (e, AST.TypeBoolean) -> T.While e (st_ body)
            _ -> error "Type of while condition must be boolean"
        U.Print expression -> case ex expression of
            (e, AST.TypeInt) -> T.Print e
            _ -> error "Type of print must be int"
        U.ExpressionStatement expression -> T.ExpressionStatement (ex_ expression)
    st = typeCheckStatement program c method
    ex = typeCheckExpression program c method
    st_ = fst . st
    ex_ = fst . ex

typeCheckExpression :: U.Program -> U.Class -> U.Method -> U.Expression -> (T.Expression, AST.Type)
typeCheckExpression program c method expression = case expression of
    U.LiteralInt value -> (T.LiteralInt value, AST.TypeInt)
    U.LiteralBoolean value -> (T.LiteralBoolean value, AST.TypeBoolean)
    U.Assignment target expression ->
        let (te, tt) = ex target
            (ee, et) = ex expression
        in if et `subtype` tt
            then case te of
                T.VariableGet name -> (T.VariableAssignment name ee, tt)
                T.MemberGet className object field -> (T.MemberAssignment className object field ee, tt)
                T.IndexGet array index -> (T.IndexAssignment array index ee, tt)
            else error "Assignment value must be a subtype"
    U.Binary l op r ->
        let (le, lt) = ex l
            (re, rt) = ex r
            logicOp = if (lt == AST.TypeBoolean && rt == AST.TypeBoolean)
                then (T.Binary le op re, AST.TypeBoolean)
                else error "Logic operands must be booleans"
            compareOp = if (lt == AST.TypeInt && rt == AST.TypeInt)
                then (T.Binary le op re, AST.TypeBoolean)
                else error "Comparison operands must be ints"
            arithOp = if (lt == AST.TypeInt && rt == AST.TypeInt)
                then (T.Binary le op re, AST.TypeInt)
                else error "Arithmetic operands must be ints"
        in case op of
            AST.Lt -> compareOp
            AST.Le -> compareOp
            AST.Eq -> compareOp
            AST.Ne -> compareOp
            AST.Gt -> compareOp
            AST.Ge -> compareOp
            AST.And -> logicOp
            AST.Or -> logicOp
            AST.Plus -> arithOp
            AST.Minus -> arithOp
            AST.Mul -> arithOp
            AST.Div -> arithOp
            AST.Mod -> arithOp
    U.Not e -> case ex e of
        (e', AST.TypeBoolean) -> (e', AST.TypeBoolean)
        _ -> error "Not operand must be boolean"
    U.IndexGet array index ->
        let (arraye, arrayt) = ex array
            (indexe, indext) = ex index
        in if arrayt == AST.TypeIntArray && indext == AST.TypeInt
            then (T.IndexGet arraye indexe, AST.TypeInt)
            else error "Array must be int[] and index must be int"
    U.Call object method args ->
        let (objecte, objectt) = ex object
            args' = map ex args
        in case objectt of
            AST.TypeObject className -> case msum [find ((== className) . (^. U.cName)) (program ^. U.pClasses) >>= find ((== method) . (^. U.mName)) . (^. U.cMethods) >>= (\a -> Just (className, a)) . id, findMethod className method] of
                Just (implementor, m) -> if length args == length (m ^. U.mParameters) && and (zipWith subtype (map snd args') (map (^. U.vType) (m ^. U.mParameters)))
                    then (T.Call implementor objecte method (map ex_ args), m ^. U.mReturnType)
                    else error "Number and types of arguments to method call must match definition"
                Nothing -> error "Method not found"
            _ -> error "Method call must be performed on an object"
    U.MemberGet object field ->
        let (objecte, objectt) = ex object
        in case objectt of
            AST.TypeObject className -> case findField className field of
                Just (className', t) -> (T.MemberGet className' objecte field, t)
                Nothing -> error "Field not found"
            AST.TypeIntArray -> if field == "length"
                then (T.IntArrayLength objecte, AST.TypeInt)
                else error "Int arrays only have a length field"
            _ -> error "Member access must be performed on an object, or length of array"
    U.VariableGet name -> case find ((== name) . (^. U.vName)) (method ^. U.mLocals ++ method ^. U.mParameters) of
        Just v -> (T.VariableGet name, v ^. U.vType)
        Nothing -> ex (U.MemberGet U.This name)
    U.This -> (T.This, AST.TypeObject (c ^. U.cName))
    U.NewIntArray size -> case ex size of
        (size', AST.TypeInt) -> (T.NewIntArray size', AST.TypeIntArray)
    U.NewObject className -> if className `elem` map (^. U.cName) (program ^. U.pClasses)
        then (T.NewObject className, AST.TypeObject className)
        else error "Class not found"
    where
    st = typeCheckStatement program c method
    ex = typeCheckExpression program c method
    st_ = fst . st
    ex_ = fst . ex
    findMethod thisClass methodName = case find ((== thisClass) . (^. U.cName)) (program ^. U.pClasses) >>= find ((== methodName) . (^. U.mName)) . (^. U.cMethods) of
        Just m -> Just (thisClass, m)
        Nothing -> case parentClassMaybe thisClass of
            Just parentClass -> findMethod (parentClass ^. U.cName) methodName
            Nothing -> Nothing
    findField thisClass field = case find ((== thisClass) . (^. U.cName)) (program ^. U.pClasses) >>= find ((== field) . (^. U.vName)) . (^. U.cFields) of
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
        thisClass <- find ((== name) . (^. U.cName)) (program ^. U.pClasses)
        find ((== (fromMaybe "Object" (thisClass ^. U.cParent))) . (^. U.cName)) (program ^. U.pClasses)

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
            parent <- vertex $ fromMaybe "Object" $ c ^. U.cParent
            return (child, parent)
        vertex = flip M.lookup m
        m = M.fromList $ zip names [0 .. ]
