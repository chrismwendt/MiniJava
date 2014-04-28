module TypeChecker where

import AST
import SSACompiler
import qualified Data.Map as M
import Control.Monad.Reader
import Safe
import Data.Maybe
import Data.Graph.Inductive (mapFst, mapSnd)

type TypeInfo = (Int, StaticType)

data TypeView = TypeView
    { getAST      :: AST.Program
    , getClassMap :: M.Map String StaticType
    , getScope    :: M.Map String StaticType
    }

assert :: Bool -> a -> a
assert False _ = error "Type error"
assert True  x = x

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

augment :: M.Map String StaticType -> AST.Program -> M.Map String StaticType
augment types (AST.Program _ classDecls) = moreTypes
    where
    pair (ClassDecl name super _ _) = (name, super)
    orphans = map (mapSnd (fromJustDef "Object") . pair) classDecls
    insertOrphan ([], m) = ([], m)
    insertOrphan (os, m) = case extract (flip M.lookup m . snd) os of
        (Nothing, _) -> error $ "Orphan classes: " ++ unwords (map fst os)
        (Just ((childName, _), parentType@(TypeObject t)), os') -> (os', M.insert childName (TypeObject (StaticTypeObject childName (Just t))) m)
        (Just (_, parentType), _) -> error $ "No class can extend " ++ show parentType
    moreTypes = snd $ until (null . fst) insertOrphan (orphans, types)

typeCheck :: SSAProgram Int -> SSAProgram TypeInfo
typeCheck program@(SSAProgram ast _ _) = runReader (tcProgram program) TypeView
    { getAST = ast
    , getClassMap = augment builtInTypes ast
    , getScope = M.empty
    }

tcProgram :: SSAProgram Int -> Reader TypeView (SSAProgram TypeInfo)
tcProgram = undefined
