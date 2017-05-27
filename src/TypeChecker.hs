{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Graph.Inductive as G
import Control.Lens
import Control.Applicative
import qualified AST as U
import qualified ASTTyped as T
import Control.Monad.Except

flub :: String -> Except String a
flub = throwError

typeCheck :: U.Program -> Except String T.Program
typeCheck = typeCheckProgram

typeCheckProgram :: U.Program -> Except String T.Program
typeCheckProgram p@(U.Program main classes) = do
    main' <- typeCheckClass p main
    classes' <- mapM (typeCheckClass p) classes
    if validClassHierarchy classes
        then return $ T.Program main' classes'
        else flub "Invalid class hierarchy"

typeCheckClass :: U.Program -> U.Class -> Except String T.Class
typeCheckClass p c@(U.Class name parent fields methods) = do
    methods' <- mapM (typeCheckMethod p c) methods
    if allUnique $ map U._mName methods
        then return $ T.Class name parent fields methods'
        else flub "Duplicate method"

typeCheckMethod :: U.Program -> U.Class -> U.Method -> Except String T.Method
typeCheckMethod p c m@(U.Method retType name ps ls ss ret) = do
    r <- tcExpT ret
    if retType == r
        then T.Method retType name ps ls <$> mapM tcSt ss <*> tcExpE ret
        else flub "Return type of method must match declaration"
    where
    tcSt = typeCheckStatement p c m
    tcExp = typeCheckExpression p c m
    tcExpE = fmap fst . tcExp
    tcExpT = fmap snd . tcExp

typeCheckStatement :: U.Program -> U.Class -> U.Method -> U.Statement -> Except String T.Statement
typeCheckStatement p c m s = s'
    where
    s' = case s of
        U.Block ss -> T.Block <$> mapM tcSt ss
        U.If condition true falseMaybe -> tcExpT condition >>= \case
            U.TypeBoolean -> T.If <$> tcExpE condition <*> tcSt true <*> (traverse tcSt falseMaybe)
            _ -> flub "Type of if condition must be boolean"
        U.While condition body -> tcExpT condition >>= \case
            U.TypeBoolean -> T.While <$> tcExpE condition <*> tcSt body
            _ -> flub "Type of while condition must be boolean"
        U.Print expression -> tcExpT expression >>= \case
            U.TypeInt -> T.Print <$> tcExpE expression
            _ -> flub "Type of print must be int"
        U.ExpressionStatement expression -> T.ExpressionStatement <$> tcExpE expression
    tcSt = typeCheckStatement p c m
    tcExp = typeCheckExpression p c m
    tcExpE = fmap fst . tcExp
    tcExpT = fmap snd . tcExp

typeCheckExpression :: U.Program -> U.Class -> U.Method -> U.Expression -> Except String (T.Expression, U.Type)
typeCheckExpression p c m e = case e of
    U.LiteralInt value -> return (T.LiteralInt value, U.TypeInt)
    U.LiteralBoolean value -> return (T.LiteralBoolean value, U.TypeBoolean)
    U.Assignment target value -> do
        (t', t'Type) <- tcExp target
        (v', v'Type) <- tcExp value
        e' <- case t' of
            T.VariableGet name -> return $ T.VariableAssignment name v'
            T.MemberGet cName object fName -> return $ T.MemberAssignment cName object fName v'
            T.IndexGet array index -> return $ T.IndexAssignment array index v'
            _ -> flub "Invalid target of assignment"
        if v'Type `subtype` t'Type
            then return (e', t'Type)
            else flub "Assignment value must be a subtype"
    U.Binary l op r -> do
        (l', l't) <- tcExp l
        (r', r't) <- tcExp r
        let checkOp lExpect rExpect resultType = if l't == lExpect && r't == rExpect
                then return (T.Binary l' op r', resultType)
                else flub "Incorrect types to binary operator"
            logicOp   = checkOp U.TypeBoolean U.TypeBoolean U.TypeBoolean
            compareOp = checkOp U.TypeInt     U.TypeInt     U.TypeBoolean
            arithOp   = checkOp U.TypeInt     U.TypeInt     U.TypeInt
        case op of
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
    U.Not e -> tcExp e >>= \case
        (e', U.TypeBoolean) -> return (T.Not e', U.TypeBoolean)
        _ -> flub "Not operand must be boolean"
    U.IndexGet array index -> do
        (array', array't) <- tcExp array
        (index', index't) <- tcExp index
        if array't == U.TypeIntArray && index't == U.TypeInt
            then return (T.IndexGet array' index', U.TypeInt)
            else flub "Array must be int[] and index must be int"
    U.Call object method args -> do
        (object', object't) <- tcExp object
        args' <- mapM tcExp args
        case object't of
            U.TypeObject cName -> do
                (implementor, m) <- getClassMethod (M.lookup cName classMap) method
                let ps = m ^. U.mParameters
                    argCountCorrect = length args == length ps
                    argTypesCorrect = and $ zipWith subtype (map snd args') (map U._vType ps)
                if argCountCorrect && argTypesCorrect
                    then return (T.Call (implementor ^. U.cName) object' method (map fst args'), m ^. U.mReturnType)
                    else flub "Number and types of arguments to method call must match definition"
            _ -> flub "Method call must be performed on an object"
    U.MemberGet object fName -> do
        (object', object't) <- tcExp object
        case object't of
            U.TypeObject cName -> do
                (c, field) <- getClassField (M.lookup cName classMap) fName
                return (T.MemberGet (c ^. U.cName) object' fName, field ^. U.vType)
            U.TypeIntArray -> if fName == "length"
                then return (T.IntArrayLength object', U.TypeInt)
                else flub "Int arrays only have a length field"
            _ -> flub "Member access must be performed on an object or length of array"
    U.VariableGet name -> case find ((== name) . U._vName) (m ^. U.mLocals ++ m ^. U.mParameters) of
        Just v -> return (T.VariableGet name, v ^. U.vType)
        Nothing -> tcExp (U.MemberGet U.This name)
    U.This -> return (T.This, U.TypeObject (c ^. U.cName))
    U.NewIntArray size -> tcExp size >>= \case
        (size', U.TypeInt) -> return (T.NewIntArray size', U.TypeIntArray)
        _ -> flub "Size of new int array must be int"
    U.NewObject cName -> if M.member cName classMap
        then return (T.NewObject cName, U.TypeObject cName)
        else flub "Class not found"

    where

    classMap = M.fromList $ map (\c -> (c ^. U.cName, c)) (p ^. U.pClasses)

    tcExp = typeCheckExpression p c m
    tcExpE = fmap fst . tcExp

    getClassField Nothing _ = flub "Class not found"
    getClassField (Just c) fName = case find (\f -> f ^. U.vName == fName) (c ^. U.cFields) of
        Just f -> return (c, f)
        Nothing -> getClassField (M.lookup (c ^. U.cParent) classMap) fName

    getClassMethod Nothing _ = flub "Method not found"
    getClassMethod (Just c) mName = case find (\m -> m ^. U.mName == mName) (c ^. U.cMethods) of
        Just m -> return (c, m)
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
