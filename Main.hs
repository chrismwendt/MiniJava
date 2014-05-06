import System.Environment
import Options.Applicative
import Parser
import qualified TypeChecker as TC
import qualified SSACompiler as SSA
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
compile (Options "type" _) source = (++ "\n") $ show $ TC.typeCheck $ parseString source
compile (Options "SSA" _) source = (++ "\n") $ show $ SSA.ssaCompile $ TC.typeCheck $ parseString source
compile (Options "reg" _) source = error "unimplemented target: reg"
compile (Options "code" _) source = error "unimplemented target: code"
compile (Options target _) _ = error $ "unknown target: " ++ target

main :: IO ()
main = execParser opts >>= \os -> do
    input <- readFile $ file os
    putStr $ compile os input
    where
    opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
