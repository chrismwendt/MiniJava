{-# LANGUAGE TemplateHaskell #-}

module TypeCheckerAST where

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
    main' = typeCheckStatement program pseudoClass pseudoMethod (program ^. U.pMain)
    classes' = map typeCheckClass (program ^. U.pClasses)
    pseudoClass = U.Class
        { U._cName = ""
        , U._cParent = Nothing
        , U._cFields = []
        , U._cMethods = []
        }
    pseudoMethod = U.Method
        { U._mReturnType = U.TypeObject ""
        , U._mName = ""
        , U._mParameters = []
        , U._mLocals = []
        , U._mStatements = []
        , U._mReturn = U.This
        }

typeCheckClass :: U.Class -> T.Class
-- typeCheckClass c = T.Class (c ^. U.cName) (fromMaybe "Object" $ c ^. U.cParent) fields methods
typeCheckClass c = undefined
    where
    fields = undefined
    methods = undefined

typeCheckStatement :: U.Program -> U.Class -> U.Method -> U.Statement -> T.Statement
typeCheckStatement = undefined

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
