module TypeChecker where

import qualified AST
import qualified ASTUntyped as ASTUntyped
import SSACompiler as SSA
import qualified Data.Map as M
import Control.Monad.State
import Safe
import Data.Maybe
import Data.Graph.Inductive (mapFst, mapSnd)
import Data.List
import qualified Data.Foldable as F

data TypeState ref = TypeState
    { getASTUntyped      :: ASTUntyped.Program
    , getClassMap :: M.Map String StaticType
    , getScope    :: M.Map String StaticType
    , getMap      :: M.Map ref (SSAStatement () ref)
    , getMap'     :: M.Map ref (SSAStatement StaticType ref)
    , getThis     :: StaticType
    }

assertType :: StaticType -> StaticType -> StaticType
assertType a b = if a == b
    then a
    else error "Type mismatch9"

unsafeFind :: (Ord k, Show k, Show v) => M.Map k v -> k -> v
unsafeFind m k = case M.lookup k m of
    Just x -> x
    Nothing -> error $ "unsafeFind: key \"" ++ show k ++ "\" not found in map:\n" ++ show m

builtInTypes :: M.Map String StaticType
builtInTypes = let object = StaticTypeObject "Object" Nothing
    in M.fromList
        [ ("int", TypeInt)
        , ("boolean", TypeBoolean)
        , ("void", TypeVoid)
        , ("int[]", TypeObject (StaticTypeObject "int[]" (Just object)))
        , ("Object", TypeObject object)
        ]

extract :: (a -> Maybe b) -> [a] -> (Maybe (a, b), [a])
extract f [] = (Nothing, [])
extract f (a:as) = case f a of
    Just b -> (Just (a, b), as)
    Nothing -> let (maybeB, rest) = extract f as in (maybeB, a:rest)

augment :: M.Map String StaticType -> ASTUntyped.Program -> M.Map String StaticType
augment types (ASTUntyped.Program _ classDecls) = moreTypes
    where
    pair (ASTUntyped.Class name super _ _) = (name, super)
    orphans = map (mapSnd (fromJustDef "Object") . pair) $ classDecls
    fail a b = error "Duplicate"
    insertOrphan ([], m) = ([], m)
    insertOrphan (os, m) = case extract (flip M.lookup m . snd) os of
        (Nothing, _) -> error $ "Orphan classes: " ++ unwords (map fst os)
        (Just ((childName, _), parentType@(TypeObject t)), os') -> (os', M.insertWith fail childName (TypeObject (StaticTypeObject childName (Just t))) m)
        (Just (_, parentType), _) -> error $ "No class can extend " ++ show parentType
    moreTypes = snd $ until (null . fst) insertOrphan (orphans, types)

typeCheck :: SSAProgram () ID -> [ID] -> M.Map ID (SSAStatement () ID) -> (SSAProgram StaticType ID, [ID], M.Map ID (SSAStatement StaticType ID))
typeCheck program@(SSAProgram ast _ _) ids m = (a, ids, getMap' s)
    where
    (a, s) = runState (tcProgram program) TypeState
        { getASTUntyped      = ast
        , getClassMap = augment builtInTypes ast
        , getScope    = M.empty
        , getMap      = m
        , getMap'     = M.empty
        , getThis     = TypeVoid
        }

tcProgram :: SSAProgram () ID -> State (TypeState ID) (SSAProgram StaticType ID)
tcProgram (SSAProgram ast@(ASTUntyped.Program s cs) sIDs classes) = do
    TypeState { getMap = m } <- get
    let mainMethod = ASTUntyped.Method (AST.TypeObject "") "" [] [] [s] ASTUntyped.This
    mapM (tcStatement mainMethod) sIDs
    classes' <- mapM tcClass classes
    return (SSAProgram ast sIDs classes')

tcClass :: SSAClass () ID -> State (TypeState ID) (SSAClass StaticType ID)
tcClass (SSAClass classDecl@(ASTUntyped.Class c _ _ _) fields methods) = do
    s@(TypeState { getClassMap = m, getThis = this }) <- get
    put $ s { getThis = unsafeFind m c }
    fields' <- mapM tcField fields
    methods' <- mapM tcMethod methods
    return $ SSAClass classDecl fields' methods'

tcField :: SSAField () -> State (TypeState ID) (SSAField StaticType)
tcField (SSAField (ASTUntyped.Variable AST.TypeBoolean name) index info) = return $ SSAField (ASTUntyped.Variable AST.TypeBoolean name) index TypeBoolean
tcField (SSAField (ASTUntyped.Variable AST.TypeInt name) index info) = return $ SSAField (ASTUntyped.Variable AST.TypeInt name) index TypeInt
tcField (SSAField (ASTUntyped.Variable AST.TypeIntArray name) index info) = do
    TypeState { getClassMap = cm } <- get
    return $ (SSAField (ASTUntyped.Variable AST.TypeIntArray name) index (unsafeFind cm "int[]"))
tcField (SSAField (ASTUntyped.Variable (AST.TypeObject super) name) index info) = do
    TypeState { getClassMap = cm } <- get
    let toStaticType AST.TypeBoolean = TypeBoolean
        toStaticType AST.TypeInt = TypeInt
        toStaticType AST.TypeIntArray = unsafeFind cm "int[]"
        toStaticType (AST.TypeObject name) = unsafeFind cm name
    return $ (SSAField (ASTUntyped.Variable (AST.TypeObject super) name) index (unsafeFind cm name))

tcMethod :: SSAMethod () ID -> State (TypeState ID) (SSAMethod StaticType ID)
tcMethod (SSAMethod methodDecl varIDs sIDs retID) = do
    mapM (tcStatement methodDecl) varIDs
    mapM (tcStatement methodDecl) sIDs
    TypeState { getClassMap = cm, getMap = m, getMap' = m' } <- get
    tcStatement methodDecl retID
    let getType id = _sInfo $ unsafeFind m' id
    let toStaticType AST.TypeBoolean = TypeBoolean
        toStaticType AST.TypeInt = TypeInt
        toStaticType AST.TypeIntArray = unsafeFind cm "int[]"
        toStaticType (AST.TypeObject name) = unsafeFind cm name
    let isSubtype a@(TypeObject (StaticTypeObject _ (Just super))) b@(TypeObject (StaticTypeObject _ _)) = if a == b then True else isSubtype (TypeObject super) b
        isSubtype a@(TypeObject (StaticTypeObject _ Nothing)) b@(TypeObject (StaticTypeObject _ _)) = if a == b then True else False
        isSubtype a b = a == b
    let subtype a b = if isSubtype a b then a else error "Type mismatch1"
    let a = isSubtype (getType retID) (toStaticType (let (ASTUntyped.Method t _ _ _ _ _) = methodDecl in t))
    if a
        then return $ SSAMethod methodDecl varIDs sIDs retID
        else error "Type mismatch2"

tcStatement :: ASTUntyped.Method -> ID -> State (TypeState ID) StaticType
tcStatement method sID = do
    TypeState { getMap = m } <- get
    let s = unsafeFind m sID
    t <- tcStatement' method s
    state@(TypeState { getMap' = m }) <- get
    put $ state { getMap' = M.insert sID (s { _sInfo = t }) m }
    return t

tcStatement' :: ASTUntyped.Method -> SSAStatement () ID -> State (TypeState ID) StaticType
tcStatement' method (SSAStatement op ()) = do
    TypeState { getASTUntyped = ast, getClassMap = cm, getMap' = m, getThis = this } <- get
    let toStaticType AST.TypeBoolean = TypeBoolean
        toStaticType AST.TypeInt = TypeInt
        toStaticType AST.TypeIntArray = unsafeFind cm "int[]"
        toStaticType (AST.TypeObject name) = unsafeFind cm name
    let getType id = _sInfo $ unsafeFind m id
    let getClass = unsafeFind cm
    let getClassDecl name = case find (\(ASTUntyped.Class cname _ _ _) -> cname == name) (let (ASTUntyped.Program _ cs) = ast in cs) of
                                Nothing -> error $ "No such class: " ++ name
                                Just c -> c
    let eqType a b = if a == b then a else error "Type mismatch3"
    let isSubtype a@(TypeObject (StaticTypeObject _ (Just super))) b@(TypeObject (StaticTypeObject _ _)) = if a == b then True else isSubtype (TypeObject super) b
        isSubtype a@(TypeObject (StaticTypeObject _ Nothing)) b@(TypeObject (StaticTypeObject _ _)) = if a == b then True else False
        isSubtype a b = a == b
    let subtype a b = if isSubtype a b then a else error "Type mismatch4"
    let assertType p t = if p then t else error "Type mismatch5"
    let logicOp l r = assertType (getType l == TypeBoolean && getType r == TypeBoolean) TypeBoolean
    let compareOp l r = assertType (getType l == TypeInt && getType r == TypeInt) TypeBoolean
    let arithOp l r = assertType (getType l == TypeInt && getType r == TypeInt) TypeInt
    let getMethod (StaticTypeObject name parent) method = (case find (\(ASTUntyped.Method _ n _ _ _ _) -> n == method) (let (ASTUntyped.Class _ _ _ ms) = getClassDecl name in ms)
                                                             of Nothing -> (case parent of
                                                                    Nothing -> error "No such method"
                                                                    Just parent -> getMethod parent method)
                                                                Just m -> m)
    let getField (StaticTypeObject name parent) field = (case find (\(ASTUntyped.Variable t n) -> n == field) (let (ASTUntyped.Class _ _ vs _) = getClassDecl name in vs)
                                                             of Nothing -> (case parent of
                                                                    Nothing -> error "No such field"
                                                                    Just parent -> getField parent field)
                                                                Just (ASTUntyped.Variable t _) -> t)
    return $ case op of
        Unify l r                    -> subtype (getType l) (getType r)
        Alias s                      -> getType s
        This                         -> this
        SSA.Variable (ASTUntyped.Variable t _) _ -> toStaticType t
        Arg arg _                    -> getType arg
        Null AST.TypeBoolean         -> TypeBoolean
        Null AST.TypeInt             -> TypeInt
        Null AST.TypeIntArray        -> getClass "int[]"
        Null (AST.TypeObject name)   -> getClass name
        SInt v                       -> TypeInt
        SBoolean v                   -> TypeBoolean
        NewObj name                  -> unsafeFind cm name
        NewIntArray s                -> unsafeFind cm "int[]"
        Label label                  -> TypeVoid
        Goto label                   -> TypeVoid
        Branch s label               -> TypeVoid
        NBranch s label              -> TypeVoid
        Call method object args      -> case getType object of
            TypeObject c@(StaticTypeObject name parent) ->
                let (ASTUntyped.Method t _ ps _ _ _) = getMethod c method
                in assertType (length ps == length args && (and $ zipWith isSubtype (map getType args) (map (\(ASTUntyped.Variable t _) -> toStaticType t) ps))) (toStaticType t)
            _ -> error "Type mismatch6"
        Print s                      -> TypeVoid
        Return s                     -> TypeVoid
        MemberGet object field          -> case getType object of
            TypeObject c@(StaticTypeObject "int[]" parent) -> if field == "length" then TypeInt else error "Bad field on int[]"
            TypeObject c@(StaticTypeObject name parent) -> toStaticType $ getField c field
            _ -> error "Type mismatch7"
        IndexGet a i                    -> assertType (getType a == getClass "int[]" && getType i == TypeInt) TypeInt
        Store s i                    -> error "Store instruction found in type checking"
        Load i                       -> error "Load instruction found in type checking"
        VarAssg id name              ->
            let (ASTUntyped.Method _ _ ps vs _ _) = method
                vars = map (\(ASTUntyped.Variable t n) -> (n, t)) vs
                pars = map (\(ASTUntyped.Variable t n) -> (n, t)) ps
                fields = let (ASTUntyped.Class _ _ fs _) = getClassDecl (let (TypeObject (StaticTypeObject n _)) = this in n) in map (\(ASTUntyped.Variable t n) -> (n, t)) fs
                t = toStaticType $ fromJust $ lookup name (vars ++ pars ++ fields)
                in if isSubtype (getType id) t then t else error "VarAssg not to subtype"
        MemberAssg object value field -> case getType object of
            TypeObject c@(StaticTypeObject name parent) -> subtype (getType value) (toStaticType (getField c field))
            _ -> error "Type mismatch8"
        IndexAssg array value index  -> assertType (getType array == getClass "int[]" && getType value == TypeInt && getType value == TypeInt) TypeInt
        Not s                        -> getType s `eqType` TypeBoolean
        Lt sl sr                     -> compareOp sl sr
        Le sl sr                     -> compareOp sl sr
        Eq sl sr                     -> compareOp sl sr
        Ne sl sr                     -> compareOp sl sr
        Gt sl sr                     -> compareOp sl sr
        Ge sl sr                     -> compareOp sl sr
        And sl sr                    -> logicOp sl sr
        Or sl sr                     -> logicOp sl sr
        Plus sl sr                   -> arithOp sl sr
        Minus sl sr                  -> arithOp sl sr
        Mul sl sr                    -> arithOp sl sr
        Div sl sr                    -> arithOp sl sr
        Mod sl sr                    -> arithOp sl sr
