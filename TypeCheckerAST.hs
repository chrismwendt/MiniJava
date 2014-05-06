{-# LANGUAGE TemplateHaskell #-}

module TypeCheckerAST where

import qualified AST
import qualified ASTUntyped as U
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
        (e', AST.TypeBoolean) -> (T.Not e', AST.TypeBoolean)
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
            AST.TypeObject cName -> case findClassMethod (M.lookup cName classMap) method of
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
            AST.TypeObject cName -> case findClassField (M.lookup cName classMap) fName of
                Just (c, field) -> (T.MemberGet (c ^. U.cName) object' fName, field ^. U.vType)
                Nothing -> error "Field not found"
            AST.TypeIntArray -> if fName == "length"
                then (T.IntArrayLength object', AST.TypeInt)
                else error "Int arrays only have a length field"
            _ -> error "Member access must be performed on an object or length of array"
    U.VariableGet name -> case find (\v -> v ^. U.vName == name) (m ^. U.mLocals ++ m ^. U.mParameters) of
        Just v -> (T.VariableGet name, v ^. U.vType)
        Nothing -> tcE (U.MemberGet U.This name)
    U.This -> (T.This, AST.TypeObject (c ^. U.cName))
    U.NewIntArray size -> case tcE size of
        (size', AST.TypeInt) -> (T.NewIntArray size', AST.TypeIntArray)
        _ -> error "Size of new int array must be int"
    U.NewObject cName -> if M.member cName classMap
        then (T.NewObject cName, AST.TypeObject cName)
        else error "Class not found"
    where
    tcS = typeCheckStatement p c m
    tcE = typeCheckExpression p c m
    tcS_ = fst . tcS
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
    subtype (AST.TypeObject name) b@(AST.TypeObject name') = if name == name'
        then True
        else case M.lookup name classMap of
            Just parentClass -> subtype (AST.TypeObject (parentClass ^. U.cName)) b
            Nothing -> False
    subtype a b = a == b

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
