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

data TypeView = TypeView
    { _ast :: U.Program
    , _environment :: M.Map String T.Type
    }
    deriving (Show)

makeLenses ''TypeView

typeCheck :: U.Program -> T.Program
typeCheck program = if validClassHierarchy (program ^. U.pClasses)
    then runReader (typeCheckProgram program) TypeView
        { _ast = program
        , _environment = M.empty
        }
    else error "Invalid class hierarchy"

typeCheckProgram :: U.Program -> Reader TypeView T.Program
typeCheckProgram program = do
    main <- typeCheckStatement (program ^. U.pMain)
    classes <- mapM typeCheckClass (program ^. U.pClasses)
    return $ T.Program main classes

typeCheckClass :: U.Class -> Reader TypeView T.Class
typeCheckClass c = do
    let fields = undefined
    let methods = undefined
    return $ T.Class (c ^. U.cName) (fromMaybe "Object" $ c ^. U.cParent) fields methods

typeCheckStatement :: U.Statement -> Reader TypeView T.Statement
typeCheckStatement s = do
    a <- ask
    return $ traceShow a undefined

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
