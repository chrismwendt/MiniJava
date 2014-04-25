import System.Environment
import Data.List
import Lexer
import Options.Applicative

data Options = Options { stopAt :: String, file :: String }

options :: Parser Options
options = Options
    <$> strOption (long "stopAt" <> metavar "[lex|parse|SSA|type|reg]")
    <*> argument str (metavar "file")

compile :: Options -> String -> String
compile (Options "lex" _) source = unlines $ map lexeme $ doLex source

main :: IO ()
main = execParser opts >>= \os -> do
    input <- readFile $ file os
    putStr $ compile os input
    where
    opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
