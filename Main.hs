import System.Environment
import Data.List
import Options.Applicative
import Parser
import AST
import SSACompiler
-- import qualified TypeChecker as TC
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
compile (Options "parse" _) source = sExpProgram $ parseString source
compile (Options "SSA" _) source = (++ "\n") $ show $ (\(p, m) -> let f ref = fromJust $ M.lookup ref m; g = id in bimap f g p) $ ssaCompile $ parseString source
-- compile (Options "type" _) source = (++ "\n") $ show $ TCA.typeCheck $ parseString source
compile (Options "reg" _) source = error "unimplemented target: reg"
compile (Options "code" _) source = error "unimplemented target: code"
compile (Options target _) _ = error $ "unknown target: " ++ target

main :: IO ()
main = execParser opts >>= \os -> do
    input <- readFile $ file os
    putStr $ compile os input
    where
    opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
