module SSACompiler where

import AST

-- data SSAProgram = SSAProgram SSAMethod [SSAClass] deriving (Show)
data SSAProgram = SSAProgram deriving (Show)

ssaCompile (Program m cs) = SSAProgram

ssaString (SSAProgram) = "hi"
