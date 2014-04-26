import System.Environment
import Data.List
import Options.Applicative
import Parser
import AST
import SSACompiler

data Options = Options { stopAt :: String, file :: String }

options :: Parser Options
options = Options
    <$> strOption (long "stopAt" <> metavar "[parse|SSA|type|reg|code]" <> value "code")
    <*> argument str (metavar "file")

compile :: Options -> String -> String
compile (Options "parse" _) source = sExpProgram $ parseString source
compile (Options "SSA" _) source = show $ ssaCompileProgram $ parseString source
compile (Options target _) _ = error $ "unknown target: " ++ target

main :: IO ()
main = execParser opts >>= \os -> do
    input <- readFile $ file os
    putStr $ compile os input
    where
    opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
