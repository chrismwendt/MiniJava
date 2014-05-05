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

typeCheck :: U.Program -> T.Program
typeCheck (U.Program { U._pMain = main, U._pClasses = classes }) = if validClassHierarchy classes
    then undefined
    else error "Invalid class hierarchy"

validClassHierarchy classes = allUnique names && all (pathTo "Object") names
    where
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

allUnique as = length (nub as) == length as
