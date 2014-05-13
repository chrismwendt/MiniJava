import System.Environment
import Options.Applicative
import Parser
import qualified TypeChecker as TC
import qualified SSACompiler as SSA
import qualified RegisterAllocator as Reg
import qualified CodeGenerator as Code
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Bifunctor

data Options = Options { stopAt :: String, file :: String }

options :: Parser Options
options = Options
    <$> strOption (long "stopAt" <> metavar "[parse|SSA|type|reg|code]" <> value "code")
    <*> argument str (metavar "file")

compile :: Options -> String -> String
compile (Options "parse" _) source = show $ parseString source
compile (Options "type" _) source = show $ TC.typeCheck $ parseString source
compile (Options "SSA" _) source = show $ SSA.compile $ TC.typeCheck $ parseString source
compile (Options "reg" _) source = show $ Reg.allocate 22 $ SSA.compile $ TC.typeCheck $ parseString source
compile (Options "code" _) source = let ast = TC.typeCheck $ parseString source in Code.generate ast $ Reg.allocate 22 $ SSA.compile $ ast
compile (Options target _) _ = error $ "unknown target: " ++ target

main :: IO ()
main = execParser opts >>= \os -> do
    input <- readFile $ file os
    putStrLn $ compile os input
    where
    opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
