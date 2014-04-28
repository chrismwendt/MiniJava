module TypeChecker where

import AST
import SSACompiler
import qualified Data.Map as M
import Control.Monad.State
import Safe

type TypeInfo = (Int, StaticType)

data TypeState = TypeState
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

augment :: M.Map String StaticType -> AST.Program -> M.Map String StaticType
augment = undefined

typeCheck :: SSAProgram Int -> SSAProgram TypeInfo
typeCheck program@(SSAProgram ast _ _) = evalState (tcProgram program) TypeState
    { getAST = ast
    , getClassMap = augment builtInTypes ast
    , getScope = M.empty
    }

tcProgram :: SSAProgram Int -> State TypeState (SSAProgram TypeInfo)
tcProgram = undefined
